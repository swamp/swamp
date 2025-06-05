/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub struct ColumnIter<'a, T> {
    grid: &'a Grid<T>,
    x: usize,
    y: usize,
}

impl<'a, T> Iterator for ColumnIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.y < self.grid.height {
            let item = &self.grid.data[self.y * self.grid.width + self.x];
            self.y += 1;
            Some(item)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Grid<T> {
    width: usize,
    height: usize,
    pub data: Vec<T>,
}

impl<T: Clone> Grid<T> {
    pub fn new(width: usize, height: usize, initial: T) -> Self {
        Self {
            width,
            height,
            data: vec![initial; width * height],
        }
    }

    #[must_use]
    pub fn column(&self, x: usize) -> Option<Vec<T>> {
        if x < self.width {
            Some(
                (0..self.height)
                    .map(|y| self.data[y * self.width + x].clone())
                    .collect(),
            )
        } else {
            None
        }
    }
}

impl<T> Grid<T> {
    #[must_use]
    pub fn get(&self, x: usize, y: usize) -> Option<&T> {
        if y < self.height && x < self.width {
            Some(&self.data[y * self.width + x])
        } else {
            None
        }
    }

    #[must_use]
    pub fn get_mut(&mut self, x: usize, y: usize) -> Option<&mut T> {
        if y < self.height && x < self.width {
            Some(&mut self.data[y * self.width + x])
        } else {
            None
        }
    }

    pub fn set(&mut self, x: usize, y: usize, data: T) -> Option<()> {
        if x < self.width && y < self.height {
            self.data[y * self.width + x] = data;
            Some(())
        } else {
            None
        }
    }

    pub fn rows(&self) -> impl Iterator<Item = &[T]> {
        self.data.chunks(self.width)
    }

    pub fn columns(&self) -> impl Iterator<Item = ColumnIter<'_, T>> {
        (0..self.width).map(move |x| ColumnIter {
            grid: self,
            x,
            y: 0,
        })
    }
}
