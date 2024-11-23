/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use int_math::UVec2;
use limnus::prelude::*;
use swamp_font::FontPlugin;
use swamp_game::{Application, GamePlugin, GameSettings};
use swamp_material::MaterialPlugin;
use swamp_render_wgpu::plugin::RenderWgpuPlugin;

pub fn run<T: Application>(
    title: &str,
    virtual_size: UVec2,
    requested_surface_size: UVec2,
) -> AppReturnValue {
    App::new()
        .insert_resource(Window {
            title: title.to_string(),
            requested_surface_size,
            minimal_surface_size: virtual_size,
            fullscreen: false,
        })
        .insert_resource(GameSettings { virtual_size })
        .add_plugins((DefaultPlugins, RenderWgpuPlugin, MaterialPlugin))
        .add_plugins(GamePlugin::<T>::new())
        .add_plugins(FontPlugin)
        .run()
}
