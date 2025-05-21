/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::idx_gen::IndexAllocator;
use crate::qck_des::quick_deserialize;
use crate::value::{QuickDeserialize, Value, to_rust_value};
use crate::value::{QuickSerialize, ValueRef};
use sparse_slot::{Id, SparseSlot};
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use swamp_types::ExternalType;
use swamp_types::Type;

#[derive(Debug, PartialEq, Eq)]
pub struct SparseValueId(pub Id);

impl Display for SparseValueId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "id:{}:{}", self.0.index, self.0.generation)
    }
}

impl QuickDeserialize for SparseValueId {
    fn quick_deserialize(octets: &[u8]) -> (Self, usize) {
        let mut offset = 0;

        // Deserialize the index
        let index = u16::from_le_bytes(octets[offset..offset + 2].try_into().unwrap());
        offset += 2;

        // Deserialize the generation
        let generation = u8::from_le_bytes(octets[offset..=offset].try_into().unwrap());
        offset += 1;

        let id = Id::new(index.into(), generation);

        (Self(id), offset)
    }
}

impl QuickSerialize for SparseValueId {
    fn quick_serialize(&self, octets: &mut [u8]) -> usize {
        let mut offset = 0;

        // Serialize the index
        let index_octets = (self.0.index as u16).to_le_bytes();
        octets[offset..offset + index_octets.len()].copy_from_slice(&index_octets);
        offset += index_octets.len();

        // Serialize the generation
        let generation_octets = self.0.generation.to_le_bytes();
        octets[offset..offset + generation_octets.len()].copy_from_slice(&generation_octets);
        offset += generation_octets.len();

        offset
    }
}

#[derive()]
pub struct SparseValueMap {
    pub sparse_slot: SparseSlot<Rc<RefCell<Value>>>,
    pub id_generator: IndexAllocator,
    pub value_item_type: Type,
    pub rust_type_ref_for_id: ExternalType,
}

impl Debug for SparseValueMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "SparseValueMap")?;

        for (id, val) in self.sparse_slot.iter() {
            writeln!(f, "  {id}: {}", val.borrow())?;
        }

        Ok(())
    }
}

impl QuickSerialize for SparseValueMap {
    fn quick_serialize(&self, octets: &mut [u8]) -> usize {
        let mut offset = 0;

        let count = self.sparse_slot.len() as u16;
        let count_octets = count.to_le_bytes();
        octets[offset..offset + count_octets.len()].copy_from_slice(&count_octets);
        offset += count_octets.len();

        for (id, value) in self.sparse_slot.iter() {
            let short_index = id.index as u16;
            let key_index_octets = short_index.to_le_bytes();
            octets[offset..offset + key_index_octets.len()].copy_from_slice(&key_index_octets);
            offset += key_index_octets.len();

            let key_generation_octets = id.generation.to_le_bytes();
            octets[offset..offset + key_generation_octets.len()]
                .copy_from_slice(&key_generation_octets);
            offset += key_generation_octets.len();

            let value_size = value.borrow().quick_serialize(&mut octets[offset..], 0);
            offset += value_size;
        }

        offset
    }
}

impl SparseValueMap {
    /// # Panics
    ///
    #[must_use]
    pub fn quick_deserialize(
        key_type: ExternalType,
        value_item_type: &Type,
        octets: &[u8],
    ) -> (Self, usize) {
        let mut sparse = Self::new(key_type, value_item_type.clone());
        let mut offset = 0;
        let count = u16::from_le_bytes(
            octets[offset..offset + 2]
                .try_into()
                .expect("could not convert to u16 count"),
        );
        offset += 2;

        for _i in 0..count {
            let index = u16::from_le_bytes(
                octets[offset..offset + 2]
                    .try_into()
                    .expect("could not convert to u16 index"),
            );
            offset += 2;

            let generation = u8::from_le_bytes(
                octets[offset..=offset]
                    .try_into()
                    .expect("could not convert to u16 generation"),
            );
            offset += 1;

            let (value, octet_size) =
                quick_deserialize(&value_item_type.clone(), &octets[offset..], 0);
            offset += octet_size;

            let id = Id::new(index as usize, generation);
            let deserialized_value_ref = Rc::new(RefCell::new(value));
            sparse
                .sparse_slot
                .try_set(id, deserialized_value_ref)
                .expect("could not insert into SparseValueMap");

            sparse.id_generator.reserve(index as usize, generation);
        }

        (sparse, offset)
    }
}

impl Display for SparseValueMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Sparse<{:?}> len:{}",
            self.value_item_type,
            self.sparse_slot.len()
        )
    }
}

impl SparseValueMap {
    #[must_use]
    pub fn new(rust_type_ref_for_id: ExternalType, value_item_type: Type) -> Self {
        /* let type_parameter = match &resolved_type {
            Type::Generic(_, parameters) => parameters[0].clone(),
            _ => panic!("illegal sparse type. not generic"),
        }; */

        Self {
            sparse_slot: SparseSlot::<Rc<RefCell<Value>>>::new(2048),
            id_generator: IndexAllocator::new(),
            value_item_type,
            rust_type_ref_for_id,
        }
    }

    /// # Panics
    ///
    pub fn add(&mut self, v: Value) -> Value {
        // TODO: Check that value is of correct type parameter
        let (index, generation) = self.id_generator.create();

        let id = Id { index, generation };

        // Always store mutable references
        let mutable_reference = Rc::new(RefCell::new(v));

        self.sparse_slot
            .try_set(id, mutable_reference)
            .expect("sparse should work");

        let script_id = SparseValueId(id);

        to_rust_value(self.rust_type_ref_for_id.clone(), script_id)
    }

    pub fn remove(&mut self, id: &SparseValueId) {
        self.id_generator.remove((id.0.index, id.0.generation));
        self.sparse_slot.remove(id.0);
    }

    #[must_use]
    pub fn get(&self, id: &SparseValueId) -> Option<&ValueRef> {
        self.sparse_slot.get(id.0)
    }

    #[allow(unused)]
    #[must_use]
    pub fn iter(&self) -> sparse_slot::Iter<'_, Rc<RefCell<Value>>> {
        self.sparse_slot.iter()
    }

    pub fn iter_mut(&mut self) -> sparse_slot::IterMut<'_, Rc<RefCell<Value>>> {
        self.sparse_slot.iter_mut()
    }

    #[must_use]
    pub fn values(&self) -> Vec<Rc<RefCell<Value>>> {
        self.sparse_slot.iter().map(|(_id, v)| v.clone()).collect()
    }
}
