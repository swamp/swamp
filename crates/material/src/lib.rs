/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use limnus::prelude::*;
use swamp_render_wgpu::{Material, Render};
use tracing::debug;

pub struct MaterialPlugin;

impl Plugin for MaterialPlugin {
    fn build(&self, app: &mut App) {
        {
            let registry = app.resource_mut::<WrappedAssetLoaderRegistry>();
            let loader = MaterialWgpuProcessor::new();

            registry.value.lock().unwrap().register_loader(loader);
        }

        app.insert_resource(Assets::<Material>::default());
    }
}

#[derive(Default)]
pub struct MaterialWgpuProcessor;

impl MaterialWgpuProcessor {
    pub fn new() -> Self {
        Self {}
    }
}

impl AssetLoader for MaterialWgpuProcessor {
    type AssetType = Material;

    fn convert_and_insert(
        &self,
        id: RawWeakId,
        octets: &[u8],
        resources: &mut ResourceStorage,
    ) -> Result<(), ConversionError> {
        let device_info = resources.fetch::<BasicDeviceInfo>();

        let name: AssetName;
        {
            let asset_container = resources.fetch::<AssetRegistry>();
            name = asset_container
                .name_raw(id)
                .expect("should know about this Id");
        }

        debug!("convert from png {name}");
        let img = image::load_from_memory_with_format(octets, image::ImageFormat::Png)
            .expect("Failed to load image");
        let img = img.to_rgba8();

        debug!("creating texture {name}");
        let wgpu_texture = swamp_wgpu_sprites::load_texture_from_memory(
            &device_info.device,
            &device_info.queue,
            img,
            name.value(),
        );

        debug!("creating material {name}");
        {
            let swamp_render_wgpu = resources.fetch_mut::<Render>();
            let wgpu_material = swamp_render_wgpu.material_from_texture(wgpu_texture, name.value());

            let image_assets = resources.fetch_mut::<Assets<Material>>();
            image_assets.set_raw(id, wgpu_material);
        }

        debug!("material complete {name}");

        Ok(())
    }
}
