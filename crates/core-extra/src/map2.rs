/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use seq_map::SeqMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

pub fn print_grid<R, C, V>(map: &Map2<R, C, V>)
where
    R: Display + Eq + Hash + Clone,
    C: Display + Eq + Hash + Clone,
    V: Debug + Clone,
{
    let rows: Vec<_> = map.rows.keys().collect();
    let cols: Vec<_> = map.columns.keys().collect();

    // Print header: leave space for row labels.
    print!("{:>10} ", ""); // empty top-left cell
    for col in &cols {
        print!("{col:>10} ");
    }
    println!();

    for row in rows {
        print!("{row:>10} ");
        for col in &cols {
            if let Some(value) = map.get(col, row) {
                print!("{value:?} ");
            } else {
                print!("{:>10} ", "");
            }
        }
        println!();
    }
}

#[derive(Debug, Clone)]
pub struct Map2<R: Eq + Hash, C: Eq + Hash, V> {
    rows: SeqMap<R, SeqMap<C, V>>,
    columns: SeqMap<C, SeqMap<R, V>>,
}

impl<R, C, V> Default for Map2<R, C, V>
where
    R: Eq + Hash + Clone,
    C: Eq + Hash + Clone,
    V: Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<R, C, V> Map2<R, C, V>
where
    R: Eq + Hash + Clone,
    C: Eq + Hash + Clone,
    V: Clone,
{
    #[must_use]
    pub fn new() -> Self {
        Self {
            rows: SeqMap::new(),
            columns: SeqMap::new(),
        }
    }

    #[must_use]
    pub const fn rows(&self) -> &SeqMap<R, SeqMap<C, V>> {
        &self.rows
    }

    #[must_use]
    pub const fn columns(&self) -> &SeqMap<C, SeqMap<R, V>> {
        &self.columns
    }

    /// Returns a reference to the value at the given row and column.
    pub fn get(&self, col: &C, row: &R) -> Option<&V> {
        self.rows.get(row).and_then(|row_map| row_map.get(col))
    }

    /// Gets a reference to the row (i.e. all columns for that row).
    pub fn get_row(&self, row: &R) -> Option<&SeqMap<C, V>> {
        self.rows.get(row)
    }

    /// Gets a reference to the column (i.e. all rows for that column).
    pub fn get_column(&self, col: &C) -> Option<&SeqMap<R, V>> {
        self.columns.get(col)
    }

    pub fn has(&self, col: &C, row: &R) -> bool {
        self.rows
            .get(row)
            .is_some_and(|row_map| row_map.contains_key(col))
    }

    /// Inserts a value into the map at the given row and column.
    /// If there was an existing value at that position, it is returned.
    pub fn insert(&mut self, col: C, row: R, value: V) {
        // Insert into the rows map.
        if self.rows.contains_key(&row) {
            self.rows
                .get_mut(&row)
                .unwrap()
                .insert(col.clone(), value.clone())
                .unwrap();
        } else {
            let mut row_map = SeqMap::new();
            row_map.insert(col.clone(), value.clone()).unwrap();
            self.rows.insert(row.clone(), row_map).unwrap();
        }

        // Insert into the columns map.
        if self.columns.contains_key(&col) {
            self.columns
                .get_mut(&col)
                .unwrap()
                .insert(row, value)
                .unwrap();
        } else {
            let mut col_map = SeqMap::new();
            col_map.insert(row, value).unwrap();
            self.columns.insert(col, col_map).unwrap();
        }
    }

    /// Removes the value at the given row and column.
    /// Returns the removed value, if it existed.
    pub fn remove(&mut self, col: &C, row: &R) -> Option<V> {
        // Remove from the rows map.
        let removed = if let Some(row_map) = self.rows.get_mut(row) {
            let removed = row_map.remove(col);
            // Clean up if the row becomes empty.
            if row_map.is_empty() {
                self.rows.remove(row);
            }
            removed
        } else {
            None
        };

        // Remove from the columns map.
        if let Some(col_map) = self.columns.get_mut(col) {
            col_map.remove(row);
            // Clean up if the column becomes empty.
            if col_map.is_empty() {
                self.columns.remove(col);
            }
        }
        removed
    }
}
