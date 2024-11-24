/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use bmf_parser::BMFont;
use int_math::{URect, UVec2, Vec2};
use limnus::prelude::*;
use tracing::debug;

pub type FontRef = Id<Font>;
pub type WeakFontRef = WeakId<Font>;

#[derive(Debug, Asset)]
pub struct Font {
    font: BMFont,
}

pub struct FontPlugin;

impl Plugin for FontPlugin {
    fn build(&self, app: &mut App) {
        {
            let registry = app.resource_mut::<WrappedAssetLoaderRegistry>();
            let loader = FontConverter::new();

            registry.value.lock().unwrap().register_loader(loader);
        }

        app.insert_resource(Assets::<Font>::default());
    }
}

#[derive(Default)]
pub struct FontConverter;

impl FontConverter {
    pub fn new() -> Self {
        Self {}
    }
}

impl AssetLoader for FontConverter {
    type AssetType = Font;

    fn convert_and_insert(
        &self,
        id: RawWeakId,
        octets: &[u8],
        resources: &mut ResourceStorage,
    ) -> Result<(), ConversionError> {
        let name: AssetName;
        {
            let asset_container = resources.fetch::<AssetRegistry>();
            name = asset_container
                .name_raw(id)
                .expect("should know about this Id");
        }

        debug!("convert from fnt {name}");
        let font = bmf_parser::BMFont::from_octets(octets)?;

        debug!("font complete {name}");
        let font_assets = resources.fetch_mut::<Assets<Font>>();

        font_assets.set_raw(id, Font { font });

        Ok(())
    }
}

#[derive(Debug)]
pub struct Glyph {
    pub relative_position: Vec2,
    pub texture_rectangle: URect,
}

impl Font {
    pub fn from_octets(bm_contents: &[u8]) -> Self {
        let font = BMFont::from_octets(bm_contents).unwrap();
        Self { font }
    }

    pub fn draw(&self, text: &str) -> Vec<Glyph> {
        let mut x = 0;
        let y = 0;
        let common = self.font.common.as_ref().unwrap();
        let height = self.font.info.as_ref().unwrap().font_size;
        let mut glyphs = Vec::new();
        let factor = 1u16;
        let y_offset = -(common.line_height as i16 - common.base as i16);
        for ch in text.chars() {
            if let Some(bm_char) = self.font.chars.get(&(ch as u32)) {
                let cx = x + bm_char.x_offset * factor as i16;
                let cy = y + y_offset + height - (bm_char.height as i16) - bm_char.y_offset;

                let glyph = Glyph {
                    relative_position: Vec2 { x: cx, y: cy },
                    texture_rectangle: URect {
                        position: UVec2 {
                            x: bm_char.x,
                            y: bm_char.y,
                        },
                        size: UVec2 {
                            x: bm_char.width,
                            y: bm_char.height,
                        },
                    },
                };
                x += bm_char.x_advance * factor as i16;

                glyphs.push(glyph);
            }
        }

        glyphs
    }
}
