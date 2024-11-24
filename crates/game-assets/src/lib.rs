use int_math::UVec2;
use limnus::prelude::*;
use monotonic_time_rs::Millis;
use std::fmt::Debug;
use swamp_font::Font;
use swamp_font::Glyph;
use swamp_render_wgpu::{FixedAtlas, FontAndMaterial, Material, MaterialRef};

pub trait Assets {
    #[must_use]
    fn now(&self) -> Millis;

    #[must_use]
    fn material_png(&mut self, name: impl Into<AssetName>) -> MaterialRef;

    #[must_use]
    fn frame_fixed_grid_material_png(
        &mut self,
        name: impl Into<AssetName>,
        grid_size: UVec2,
        texture_size: UVec2,
    ) -> FixedAtlas;

    #[must_use]
    fn bm_font(&mut self, name: impl Into<AssetName>) -> FontAndMaterial;

    #[must_use]
    fn text_glyphs(&self, text: &str, font_and_mat: &FontAndMaterial) -> Option<Vec<Glyph>>;

    #[must_use]
    fn font(&self, font_ref: &Id<Font>) -> Option<&Font>;
    #[must_use]
    fn audio_sample_wav(&mut self, name: impl Into<AssetName>) -> StereoSampleRef;
}

pub struct GameAssets<'a> {
    now: Millis,
    resource_storage: &'a mut ResourceStorage,
}

impl Debug for GameAssets<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "assets")
    }
}

impl<'a> GameAssets<'a> {
    pub fn new(resource_storage: &'a mut ResourceStorage, now: Millis) -> Self {
        Self {
            resource_storage,
            now,
        }
    }
}

impl Assets for GameAssets<'_> {
    fn now(&self) -> Millis {
        self.now
    }

    fn material_png(&mut self, name: impl Into<AssetName>) -> MaterialRef {
        let asset_loader = self
            .resource_storage
            .get_mut::<AssetRegistry>()
            .expect("should exist registry");
        asset_loader.load::<Material>(name.into().with_extension("png"))
    }

    fn frame_fixed_grid_material_png(
        &mut self,
        name: impl Into<AssetName>,
        grid_size: UVec2,
        texture_size: UVec2,
    ) -> FixedAtlas {
        let material_ref = self.material_png(name);

        FixedAtlas::new(grid_size, texture_size, material_ref)
    }

    fn bm_font(&mut self, name: impl Into<AssetName>) -> FontAndMaterial {
        let asset_name = name.into();
        let asset_loader = self
            .resource_storage
            .get_mut::<AssetRegistry>()
            .expect("should exist registry");
        let font_ref = asset_loader.load::<Font>(asset_name.clone().with_extension("fnt"));
        let material_ref = asset_loader.load::<Material>(asset_name.clone().with_extension("png"));

        FontAndMaterial {
            font_ref,
            material_ref,
        }
    }

    fn text_glyphs(&self, text: &str, font_and_mat: &FontAndMaterial) -> Option<Vec<Glyph>> {
        if let Some(font) = self.font(&font_and_mat.font_ref) {
            let glyphs = font.draw(text);
            Some(glyphs)
        } else {
            None
        }
    }

    fn font(&self, font_ref: &Id<Font>) -> Option<&Font> {
        let font_assets = self
            .resource_storage
            .get::<limnus::prelude::Assets<Font>>()
            .expect("font assets should be a thing");

        font_assets.get(font_ref)
    }

    fn audio_sample_wav(&mut self, name: impl Into<AssetName>) -> StereoSampleRef {
        let asset_loader = self
            .resource_storage
            .get_mut::<AssetRegistry>()
            .expect("should exist registry");
        asset_loader.load::<StereoSample>(name.into().with_extension("wav"))
    }
}
