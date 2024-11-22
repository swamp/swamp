/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp::prelude::*;

const TILE_SIZE: UVec2 = UVec2::new(32, 32);
const VIRTUAL_SCREEN_SIZE: UVec2 = UVec2::new(320 * 2, 240 * 2);
const START_WINDOW_SIZE: UVec2 = UVec2::new(1280, 800);

#[derive(Debug)]
pub struct SingleTextureExample {
    fantasy_tileset_atlas: FixedAtlas,
    test: MaterialRef,
    tick_count: u32,
}

impl SingleTextureExample {
    fn fun_value(&self, speed: f32, min: i16, max: i16, offset: u32) -> i16 {
        let angle = (self.tick_count + offset) as f32 * 0.1 * speed;
        let sin_value = angle.sin();

        let pos_sin = (sin_value + 1.0) / 2.0;
        let pos_sin_int = (pos_sin * (max - min) as f32) as u16;

        pos_sin_int as i16 + min
    }
}

impl Application for SingleTextureExample {
    fn new(assets: &mut impl Assets) -> Self {
        let fantasy_tileset_atlas = assets.frame_fixed_grid_material_png(
            "fantasy-tileset",
            TILE_SIZE,
            UVec2::new(256, 832),
        );

        let test = assets.material_png("fantasy-tileset");

        Self {
            fantasy_tileset_atlas,
            test,
            tick_count: 0,
        }
    }

    fn tick(&mut self, _assets: &mut impl Assets) {
        self.tick_count += 1;
    }

    fn render(&mut self, gfx: &mut impl Gfx) {
        gfx.draw_sprite(
            (
                VIRTUAL_SCREEN_SIZE.x as i16 / 2i16,
                VIRTUAL_SCREEN_SIZE.y as i16 / 2i16,
                0,
            )
                .into(),
            UVec2::new(256, 832),
            &self.test,
        );
        gfx.sprite_atlas_frame(
            (
                VIRTUAL_SCREEN_SIZE.x as i16 / 2i16,
                VIRTUAL_SCREEN_SIZE.y as i16 / 2i16,
                0,
            )
                .into(),
            16,
            &self.fantasy_tileset_atlas,
        );

        let pan = self.fun_value(
            0.05,
            -((VIRTUAL_SCREEN_SIZE.x - 10) as i16),
            (VIRTUAL_SCREEN_SIZE.x - 10) as i16,
            self.tick_count,
        );

        gfx.set_origin((pan, 0).into());

        const COLS: u16 = VIRTUAL_SCREEN_SIZE.x / TILE_SIZE.x;
        for frame_index in 0..128u16 {
            gfx.sprite_atlas_frame(
                (
                    (frame_index % COLS) as i16 * (TILE_SIZE.x as i16)
                        + (frame_index % COLS) as i16,
                    self.fun_value(
                        0.15,
                        10,
                        VIRTUAL_SCREEN_SIZE.y as i16,
                        frame_index as u32 * 12,
                    ),
                    frame_index as i16,
                )
                    .into(),
                (frame_index % 128) + 16,
                &self.fantasy_tileset_atlas,
            );
        }
    }
}

fn main() {
    run::<SingleTextureExample>(
        "Single Texture Example",
        VIRTUAL_SCREEN_SIZE,
        START_WINDOW_SIZE,
    );
}
