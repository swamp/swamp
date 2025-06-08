/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;
use swamp_types::prelude::*;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum IntrinsicFunction {
    // Bool
    BoolToString,

    // Int
    IntAbs,
    IntRnd,
    IntMax,
    IntMin,
    IntClamp,
    IntToFloat,
    IntToString,

    // Float
    FloatRound,
    FloatFloor,
    FloatSqrt,
    FloatSign,
    FloatAbs,
    FloatRnd,
    FloatCos,
    FloatSin,
    FloatAcos,
    FloatAsin,
    FloatAtan2,
    FloatMin,
    FloatMax,
    FloatClamp,
    FloatToString,

    // String
    StringLen,
    // TODO: StringSubscript, StringConcat

    // Vec
    VecPush,
    VecPop,
    VecRemoveIndex,
    VecRemoveIndexGetValue,
    VecLen,
    VecClear,
    VecSwap,
    VecInsert,
    VecFirst,
    VecGet,
    VecLast,

    VecFor,
    VecWhile,
    VecFindMap,
    VecAny,
    VecAll,
    VecMap,
    VecFilter,
    VecFilterMap,
    VecFind,
    VecFold,
    VecCapacity,
    VecIsEmpty,

    // Map
    MapIsEmpty,
    MapHas,
    MapRemove,
    MapIter,
    MapIterMut,
    MapLen,

    // Grid
    GridCreate,
    GridFromSlice,
    GridGetColumn,
    GridSet,
    GridGet,

    Float2Magnitude,

    RuntimePanic,
    RuntimeHalt,
    RuntimeStep,
    RangeInit,
    MapCapacity,
}

pub type IntrinsicFunctionDefinitionRef = Rc<IntrinsicFunctionDefinition>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IntrinsicFunctionDefinition {
    pub name: String,
    pub signature: Signature,
    pub intrinsic: IntrinsicFunction,
}

impl fmt::Display for IntrinsicFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            // Bool
            Self::BoolToString => "bool_to_string",

            // Float
            Self::FloatRound => "float_round",
            Self::FloatFloor => "float_floor",
            Self::FloatSqrt => "float_sqrt",
            Self::FloatSign => "float_sign",
            Self::FloatAbs => "float_abs",
            Self::FloatRnd => "float_rnd",
            Self::FloatCos => "float_cos",
            Self::FloatSin => "float_sin",
            Self::FloatAcos => "float_acos",
            Self::FloatAsin => "float_asin",
            Self::FloatAtan2 => "float_atan2",
            Self::FloatMin => "float_min",
            Self::FloatMax => "float_max",
            Self::FloatClamp => "float_clamp",
            Self::FloatToString => "float_to_string",

            // Int
            Self::IntAbs => "int_abs",
            Self::IntRnd => "int_rnd",
            Self::IntMax => "int_max",
            Self::IntMin => "int_min",
            Self::IntToFloat => "int_to_float",
            Self::IntClamp => "int_clamp",
            Self::IntToString => "int_to_string",

            // String
            Self::StringLen => "string_len",

            // Vec
            Self::VecPush => "vec_push",
            Self::VecPop => "vec_pop",
            Self::VecRemoveIndex => "vec_remove_index",
            Self::VecRemoveIndexGetValue => "vec_remove_index_get_value",
            Self::VecClear => "vec_clear",
            Self::VecGet => "vec_get",
            Self::VecFor => "vec_for",
            Self::VecIsEmpty => "vec_is_empty",
            Self::VecWhile => "vec_while",
            Self::VecFindMap => "vec_find_map",
            Self::VecLen => "vec_len",
            Self::VecCapacity => "vec_capacity",
            Self::VecAny => "vec_any",
            Self::VecAll => "vec_all",
            Self::VecMap => "vec_map",
            Self::VecFilter => "vec_filter",
            Self::VecFilterMap => "vec_filter_map",
            Self::VecFind => "vec_find",
            Self::VecFold => "vec_fold",
            Self::VecSwap => "vec_swap",
            Self::VecInsert => "vec_insert",
            Self::VecFirst => "vec_first",
            Self::VecLast => "vec_last",

            // Map
            Self::MapHas => "map_has",
            Self::MapRemove => "map_remove",
            Self::MapIter => "map_iter",
            Self::MapIterMut => "map_iter_mut",
            Self::MapLen => "map_len",
            Self::MapCapacity => "map_len",
            Self::MapIsEmpty => "map_is_empty",

            // Grid
            Self::GridCreate => "grid_new",
            Self::GridSet => "grid_set",
            Self::GridGet => "grid_get",
            Self::GridFromSlice => "grid_from_slice",
            Self::GridGetColumn => "grid_get_column",

            // Other
            Self::Float2Magnitude => "float2_magnitude",
            Self::RuntimePanic => "runtime_panic",
            Self::RuntimeHalt => "runtime_halt",
            Self::RuntimeStep => "runtime_step",
            Self::RangeInit => "rinit",
        };

        write!(f, "{name}")
    }
}
