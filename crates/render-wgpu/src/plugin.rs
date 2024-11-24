/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Render;
use limnus::prelude::*;
use monotonic_time_rs::Millis;
use std::sync::Arc;
use tracing::debug;

fn tick(mut wgpu_render: ReM<Render>, window_messages: Msg<WindowMessage>) {
    for msg in window_messages.iter_previous() {
        if let WindowMessage::Resized(size) = msg {
            debug!("wgpu_render detected resized to {:?}", size);
            wgpu_render.resize(*size)
        }
    }
}

pub struct RenderWgpuPlugin;

impl Plugin for RenderWgpuPlugin {
    fn post_initialization(&self, app: &mut App) {
        let window = app.resource::<WgpuWindow>();
        let window_settings = app.resource::<Window>();
        let wgpu_render = Render::new(
            Arc::clone(window.device()),
            Arc::clone(window.queue()),
            window.texture_format(),
            window_settings.requested_surface_size,
            window_settings.minimal_surface_size,
            Millis::new(0),
        );
        app.insert_resource(wgpu_render);
        app.add_system(UpdatePhase::First, tick);
    }
}
