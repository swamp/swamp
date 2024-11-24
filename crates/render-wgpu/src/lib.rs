/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod plugin;
pub mod prelude;

use int_math::{URect, UVec2, Vec2, Vec3};
use limnus::prelude::*;
use monotonic_time_rs::Millis;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::sync::Arc;
use swamp_font::Font;
use swamp_font::FontRef;
use swamp_font::WeakFontRef;
use swamp_render::prelude::*;
use swamp_wgpu_sprites::{SpriteInfo, SpriteInstanceUniform};
use tracing::trace;
use wgpu::{BindGroup, BindGroupLayout, Buffer, RenderPass, RenderPipeline};

pub type MaterialRef = Id<Material>;
pub type WeakMaterialRef = WeakId<Material>;

pub trait FrameLookup {
    fn lookup(&self, frame: u16) -> (&MaterialRef, URect);
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FixedAtlas {
    pub material: MaterialRef,
    pub texture_size: UVec2,
    pub one_cell_size: UVec2,
    pub cell_count_size: UVec2,
}

impl FixedAtlas {
    pub fn new(one_cell_size: UVec2, texture_size: UVec2, material_ref: MaterialRef) -> Self {
        let cell_count_size = UVec2::new(
            texture_size.x / one_cell_size.x,
            texture_size.y / one_cell_size.y,
        );

        if cell_count_size.x == 0 {
            panic!("illegal texture and one cell size");
        }

        Self {
            material: material_ref,
            texture_size,
            one_cell_size,
            cell_count_size,
        }
    }
}

impl FrameLookup for FixedAtlas {
    fn lookup(&self, frame: u16) -> (&MaterialRef, URect) {
        let x = frame % self.cell_count_size.x;
        let y = frame / self.cell_count_size.x;

        (
            &self.material,
            URect::new(
                x * self.one_cell_size.x,
                y * self.one_cell_size.y,
                self.one_cell_size.x,
                self.one_cell_size.y,
            ),
        )
    }
}

#[derive(Debug)]
pub struct FontAndMaterial {
    pub font_ref: FontRef,
    pub material_ref: MaterialRef,
}

pub trait Gfx {
    fn sprite_atlas_frame(&mut self, position: Vec3, frame: u16, atlas: &impl FrameLookup);
    fn sprite_atlas(&mut self, position: Vec3, atlas_rect: URect, material_ref: &MaterialRef);
    fn draw_sprite(&mut self, position: Vec3, size: UVec2, material_ref: &MaterialRef);
    fn set_origin(&mut self, position: Vec2);

    fn set_clear_color(&mut self, color: Color);

    fn tilemap_params(
        &mut self,
        position: Vec3,
        tiles: &[u16],
        width: u16,
        atlas_ref: &FixedAtlas,
        scale: u8,
    );

    fn text_draw(&mut self, position: Vec3, text: &str, font_ref: &FontAndMaterial);

    #[must_use]
    fn now(&self) -> Millis;

    #[must_use]
    fn physical_aspect_ratio(&self) -> AspectRatio;

    #[must_use]
    fn physical_size(&self) -> UVec2;

    fn set_viewport(&mut self, viewport_strategy: ViewportStrategy);

    #[must_use]
    fn viewport(&self) -> &ViewportStrategy;

    fn set_scale(&mut self, scale_factor: VirtualScale);
}

fn to_wgpu_color(c: Color) -> wgpu::Color {
    let f = c.to_f64();
    wgpu::Color {
        r: f.0,
        g: f.1,
        b: f.2,
        a: f.3,
    }
}

struct RenderItem {
    position: Vec3,
    material_ref: WeakMaterialRef,

    renderable: Renderable,
}

pub struct Text {
    text: String,
    font_ref: WeakFontRef,
}

enum Renderable {
    Sprite(Sprite),
    TileMap(TileMap),
    Text(Text),
}

#[derive(Resource)]
pub struct Render {
    index_buffer: Buffer,  // Only indicies for a single identity quad
    vertex_buffer: Buffer, // Only one identity quad (0,0,1,1)
    sampler: wgpu::Sampler,
    pipeline: RenderPipelineRef,
    physical_surface_size: UVec2,
    viewport_strategy: ViewportStrategy,
    // Group 0
    camera_bind_group: BindGroup,
    #[allow(unused)]
    camera_buffer: Buffer,

    // Group 1
    texture_sampler_bind_group_layout: BindGroupLayout,

    // Group 1
    quad_matrix_and_uv_instance_buffer: Buffer,

    device: Arc<wgpu::Device>,
    queue: Arc<wgpu::Queue>, // Queue to talk to device

    // Internals
    items: Vec<RenderItem>,
    //fonts: Vec<FontAndMaterialRef>,
    origin: Vec2,

    // Cache
    batch_offsets: Vec<(WeakMaterialRef, u32, u32)>,
    viewport: URect,
    clear_color: wgpu::Color,
    last_render_at: Millis,
    scale: f32,
}

impl Debug for Render {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Render")
    }
}

impl Gfx for Render {
    fn sprite_atlas_frame(&mut self, position: Vec3, frame: u16, atlas: &impl FrameLookup) {
        self.sprite_atlas_frame(position, frame, atlas);
    }

    fn sprite_atlas(&mut self, position: Vec3, atlas_rect: URect, material_ref: &MaterialRef) {
        self.sprite_atlas(position, atlas_rect, material_ref);
    }

    fn draw_sprite(&mut self, position: Vec3, size: UVec2, material_ref: &MaterialRef) {
        self.draw_sprite(position, size, material_ref);
    }

    fn set_origin(&mut self, position: Vec2) {
        self.origin = position;
    }

    fn set_clear_color(&mut self, color: Color) {
        self.clear_color = to_wgpu_color(color);
    }

    fn tilemap_params(
        &mut self,
        position: Vec3,
        tiles: &[u16],
        width: u16,
        atlas_ref: &FixedAtlas,
        scale: u8,
    ) {
        self.items.push(RenderItem {
            position,
            material_ref: (&atlas_ref.material).into(),
            renderable: Renderable::TileMap(TileMap {
                tiles_data_grid_size: UVec2::new(width, tiles.len() as u16 / width),
                cell_count_size: atlas_ref.cell_count_size,
                one_cell_size: atlas_ref.one_cell_size,
                tiles: Vec::from(tiles),
                scale,
            }),
        });
    }

    fn text_draw(&mut self, position: Vec3, text: &str, font_and_mat: &FontAndMaterial) {
        self.items.push(RenderItem {
            position,
            material_ref: (&font_and_mat.material_ref).into(),
            renderable: Renderable::Text(Text {
                text: text.to_string(),
                font_ref: (&font_and_mat.font_ref).into(),
            }),
        });
    }

    fn now(&self) -> Millis {
        self.last_render_at
    }

    fn physical_aspect_ratio(&self) -> AspectRatio {
        self.physical_surface_size.into()
    }

    fn physical_size(&self) -> UVec2 {
        self.physical_surface_size
    }

    fn set_viewport(&mut self, viewport_strategy: ViewportStrategy) {
        self.viewport_strategy = viewport_strategy;
    }

    fn viewport(&self) -> &ViewportStrategy {
        &self.viewport_strategy
    }

    fn set_scale(&mut self, scale_factor: VirtualScale) {
        match scale_factor {
            VirtualScale::IntScale(scale) => self.scale = scale as f32,
            VirtualScale::FloatScale(scale) => self.scale = scale,
        }
    }
}

/*

impl Gfx for Render {

    fn set_origin(&mut self, position: Vec2) {
        self.origin = position;
    }

    fn sprite_atlas_frame(&mut self, position: Vec3, frame: u16, atlas: &impl FrameLookup) {
        self.sprite_atlas_frame(position, frame, atlas);
    }

    fn sprite_atlas(&mut self, position: Vec3, atlas_rect: URect, material: &MaterialRef) {
        self.sprite_atlas(position, atlas_rect, material);
    }




    fn tilemap(&mut self, position: Vec3, tiles: &[u16], width: u16, atlas_ref: &FixedAtlas) {
        self.items.push(RenderItem {
            position,
            material_ref: atlas_ref.material.clone(),
            renderable: Renderable::TileMap(TileMap {
                tiles_data_grid_size: UVec2::new(width, tiles.len() as u16 / width),
                cell_count_size: atlas_ref.cell_count_size,
                one_cell_size: atlas_ref.one_cell_size,
                tiles: Vec::from(tiles),
                scale: 1,
            }),
        });
    }


}

*/

impl Render {
    #[must_use]
    pub fn new(
        device: Arc<wgpu::Device>,
        queue: Arc<wgpu::Queue>, // Queue to talk to device
        surface_texture_format: wgpu::TextureFormat,
        physical_size: UVec2,
        virtual_surface_size: UVec2,
        now: Millis,
    ) -> Self {
        let (vertex_shader_source, fragment_shader_source) = sources();

        let sprite_info = SpriteInfo::new(
            &device,
            surface_texture_format,
            vertex_shader_source,
            fragment_shader_source,
            create_view_uniform_view_projection_matrix(physical_size),
        );

        Self {
            device,
            queue,
            items: Vec::new(),
            //   fonts: Vec::new(),
            sampler: sprite_info.sampler,
            pipeline: Arc::new(sprite_info.sprite_pipeline),
            texture_sampler_bind_group_layout: sprite_info.sprite_texture_sampler_bind_group_layout,
            index_buffer: sprite_info.index_buffer,
            vertex_buffer: sprite_info.vertex_buffer,
            quad_matrix_and_uv_instance_buffer: sprite_info.quad_matrix_and_uv_instance_buffer,
            camera_bind_group: sprite_info.camera_bind_group,
            batch_offsets: Vec::new(),
            camera_buffer: sprite_info.camera_uniform_buffer,
            viewport: Self::viewport_from_integer_scale(physical_size, virtual_surface_size),
            clear_color: to_wgpu_color(Color::from_f32(0.008, 0.015, 0.008, 1.0)),
            origin: Vec2::new(0, 0),
            last_render_at: now,
            physical_surface_size: physical_size,
            viewport_strategy: ViewportStrategy::FitIntegerScaling(virtual_surface_size),
            scale: 1.0,
        }
    }

    pub fn set_now(&mut self, now: Millis) {
        self.last_render_at = now;
    }

    pub const fn virtual_surface_size(&self) -> UVec2 {
        match self.viewport_strategy {
            ViewportStrategy::FitIntegerScaling(virtual_size)
            | ViewportStrategy::FitFloatScaling(virtual_size) => virtual_size,
            ViewportStrategy::MatchPhysicalSize => self.physical_surface_size,
        }
    }

    pub const fn physical_surface_size(&self) -> UVec2 {
        self.physical_surface_size
    }

    pub const fn viewport(&self) -> URect {
        self.viewport
    }

    #[inline(always)]
    fn push_sprite(&mut self, position: Vec3, material: &MaterialRef, sprite: Sprite) {
        self.items.push(RenderItem {
            position,
            material_ref: material.into(),
            renderable: Renderable::Sprite(sprite),
        });
    }

    pub fn viewport_from_integer_scale(physical_size: UVec2, virtual_size: UVec2) -> URect {
        let window_aspect = physical_size.x as f32 / physical_size.y as f32;
        let virtual_aspect = virtual_size.x as f32 / virtual_size.y as f32;

        if physical_size.x < virtual_size.x || physical_size.y < virtual_size.y {
            return URect::new(0, 0, physical_size.x, physical_size.y);
        }

        let mut integer_scale = if window_aspect > virtual_aspect {
            physical_size.y / virtual_size.y
        } else {
            physical_size.x / virtual_size.x
        };

        if integer_scale < 1 {
            integer_scale = 1;
        }

        let viewport_actual_size = UVec2::new(
            virtual_size.x * integer_scale,
            virtual_size.y * integer_scale,
        );

        let border_size = physical_size - viewport_actual_size;

        let offset = border_size / 2;

        URect::new(
            offset.x,
            offset.y,
            viewport_actual_size.x,
            viewport_actual_size.y,
        )
    }

    pub fn viewport_from_float_scale(physical_size: UVec2, virtual_size: UVec2) -> URect {
        let window_aspect = physical_size.x as f32 / physical_size.y as f32;
        let virtual_aspect = virtual_size.x as f32 / virtual_size.y as f32;

        if physical_size.x < virtual_size.x || physical_size.y < virtual_size.y {
            return URect::new(0, 0, physical_size.x, physical_size.y);
        }

        let mut float_scale = if window_aspect > virtual_aspect {
            physical_size.y as f32 / virtual_size.y as f32
        } else {
            physical_size.x as f32 / virtual_size.x as f32
        };

        if float_scale < 0.01 {
            float_scale = 0.01;
        }

        let viewport_actual_size = UVec2::new(
            (virtual_size.x as f32 * float_scale) as u16,
            (virtual_size.y as f32 * float_scale) as u16,
        );

        let border_size = physical_size - viewport_actual_size;

        let offset = border_size / 2;

        URect::new(
            offset.x,
            offset.y,
            viewport_actual_size.x,
            viewport_actual_size.y,
        )
    }

    pub fn resize(&mut self, physical_size: UVec2) {
        self.physical_surface_size = physical_size;
    }

    /*
    pub fn render_sprite(&mut self, position: Vec3, material: &MaterialRef, params: SpriteParams) {
        let atlas_rect = URect::new(0, 0, material.texture_size().x, material.texture_size().y);

        self.push_sprite(position, material, Sprite { atlas_rect, params });
    }*/

    pub fn sprite_atlas(&mut self, position: Vec3, atlas_rect: URect, material_ref: &MaterialRef) {
        self.push_sprite(
            position,
            material_ref,
            Sprite {
                atlas_rect,
                params: Default::default(),
            },
        );
    }

    fn sprite_atlas_frame(&mut self, position: Vec3, frame: u16, atlas: &impl FrameLookup) {
        let (material_ref, atlas_rect) = atlas.lookup(frame);
        self.push_sprite(
            position,
            material_ref,
            Sprite {
                atlas_rect,
                params: Default::default(),
            },
        );
    }

    fn draw_sprite(&mut self, position: Vec3, size: UVec2, material: &MaterialRef) {
        self.push_sprite(
            position,
            material,
            Sprite {
                atlas_rect: URect::new(0, 0, size.x, size.y),
                params: SpriteParams {
                    ..Default::default()
                },
            },
        );
    }

    pub const fn clear_color(&self) -> wgpu::Color {
        self.clear_color
    }

    // first two is multiplier and second pair is offset
    fn calculate_texture_coords_mul_add(atlas_rect: URect, texture_size: UVec2) -> Vec4 {
        let x = atlas_rect.position.x as f32 / texture_size.x as f32;
        let y = atlas_rect.position.y as f32 / texture_size.y as f32;
        let width = atlas_rect.size.x as f32 / texture_size.x as f32;
        let height = atlas_rect.size.y as f32 / texture_size.y as f32;
        Vec4([width, height, x, y])
    }

    fn order_render_items_in_batches(&self) -> Vec<Vec<&RenderItem>> {
        let mut material_batches: Vec<Vec<&RenderItem>> = Vec::new();
        let mut current_batch: Vec<&RenderItem> = Vec::new();
        let mut current_material: Option<&WeakMaterialRef> = None;

        for sprite in &self.items {
            if Some(&sprite.material_ref) != current_material {
                if !current_batch.is_empty() {
                    material_batches.push(current_batch.clone());
                    current_batch.clear();
                }
                current_material = Some(&sprite.material_ref);
            }
            current_batch.push(sprite);
        }

        if !current_batch.is_empty() {
            material_batches.push(current_batch);
        }

        material_batches
    }

    pub fn prepare_render(&mut self, materials: &Assets<Material>, fonts: &Assets<Font>) {
        sort_render_items_by_z_and_material(&mut self.items);

        let batches = self.order_render_items_in_batches();

        let mut quad_matrix_and_uv: Vec<SpriteInstanceUniform> = Vec::new();
        let mut batch_vertex_ranges: Vec<(WeakMaterialRef, u32, u32)> = Vec::new();

        for render_items in &batches {
            let quad_index = quad_matrix_and_uv.len() as u32;
            let mut quad_count = 0;

            // Fix: Access material_ref through reference and copy it
            let weak_material_ref = render_items
                .first()
                .map(|item| {
                    // Force copy semantics by dereferencing the shared reference
                    let material_ref: WeakId<Material> = item.material_ref;
                    material_ref
                })
                .expect("Render items batch was empty");

            let result = materials.get_weak(weak_material_ref);
            if result.is_none() {
                // Material is not loaded yet
                continue;
            }
            let material = result.unwrap();
            let current_texture_size = material.texture_size;

            for render_item in render_items {
                match &render_item.renderable {
                    Renderable::Sprite(ref sprite) => {
                        let size = sprite.atlas_rect.size;
                        let render_atlas = sprite.atlas_rect;
                        let model_matrix =
                            Matrix4::from_translation(
                                render_item.position.x as f32,
                                render_item.position.y as f32,
                                0.0,
                            ) * Matrix4::from_scale(size.x as f32, size.y as f32, 1.0);
                        let tex_coords_mul_add = Self::calculate_texture_coords_mul_add(
                            render_atlas,
                            current_texture_size,
                        );

                        let quad_instance =
                            SpriteInstanceUniform::new(model_matrix, tex_coords_mul_add);
                        quad_matrix_and_uv.push(quad_instance);
                        quad_count += 1;
                    }

                    Renderable::Text(font_and_mat) => {
                        let result = fonts.get_weak(font_and_mat.font_ref);
                        if result.is_none() {
                            continue;
                        }
                        let font = result.unwrap();

                        let glyphs = font.draw(&font_and_mat.text);
                        for glyph in glyphs {
                            let pos = render_item.position + Vec3::from(glyph.relative_position);
                            let texture_size = glyph.texture_rectangle.size;
                            let model_matrix =
                                Matrix4::from_translation(pos.x as f32, pos.y as f32, 0.0)
                                    * Matrix4::from_scale(
                                        texture_size.x as f32,
                                        texture_size.y as f32,
                                        1.0,
                                    );
                            let tex_coords_mul_add = Self::calculate_texture_coords_mul_add(
                                glyph.texture_rectangle,
                                current_texture_size,
                            );

                            let quad_instance =
                                SpriteInstanceUniform::new(model_matrix, tex_coords_mul_add);
                            quad_matrix_and_uv.push(quad_instance);
                            quad_count += 1;
                        }
                    }

                    Renderable::TileMap(ref tile_map) => {
                        for (index, tile) in tile_map.tiles.iter().enumerate() {
                            let cell_pos_x = (index as u16 % tile_map.tiles_data_grid_size.x)
                                * tile_map.one_cell_size.x
                                * tile_map.scale as u16;
                            let cell_pos_y = (index as u16 / tile_map.tiles_data_grid_size.x)
                                * tile_map.one_cell_size.y
                                * tile_map.scale as u16;
                            let cell_x = *tile % tile_map.cell_count_size.x;
                            let cell_y = *tile / tile_map.cell_count_size.x;

                            let tex_x = cell_x * tile_map.one_cell_size.x;
                            let tex_y = cell_y * tile_map.one_cell_size.x;

                            let cell_texture_area = URect::new(
                                tex_x,
                                tex_y,
                                tile_map.one_cell_size.x,
                                tile_map.one_cell_size.y,
                            );

                            let cell_model_matrix = Matrix4::from_translation(
                                (render_item.position.x + cell_pos_x as i16) as f32,
                                (render_item.position.y + cell_pos_y as i16) as f32,
                                0.0,
                            ) * Matrix4::from_scale(
                                (tile_map.one_cell_size.x * tile_map.scale as u16) as f32,
                                (tile_map.one_cell_size.y * tile_map.scale as u16) as f32,
                                1.0,
                            );

                            let cell_tex_coords_mul_add = Self::calculate_texture_coords_mul_add(
                                cell_texture_area,
                                current_texture_size,
                            );

                            let quad_instance = SpriteInstanceUniform::new(
                                cell_model_matrix,
                                cell_tex_coords_mul_add,
                            );
                            quad_matrix_and_uv.push(quad_instance);
                            quad_count += 1;
                        }
                    }
                }

                batch_vertex_ranges.push((weak_material_ref, quad_index, quad_count));
            }
        }

        // write all model_matrix and uv_coords to instance buffer once, before the render pass
        self.queue.write_buffer(
            &self.quad_matrix_and_uv_instance_buffer,
            0,
            bytemuck::cast_slice(&quad_matrix_and_uv),
        );

        self.batch_offsets = batch_vertex_ranges;
    }

    pub fn render(
        &mut self,
        render_pass: &mut RenderPass,
        materials: &Assets<Material>,
        fonts: &Assets<Font>,
        now: Millis,
    ) {
        trace!("start render()");
        self.last_render_at = now;

        self.viewport = match self.viewport_strategy {
            ViewportStrategy::FitIntegerScaling(virtual_surface_size) => {
                Self::viewport_from_integer_scale(self.physical_surface_size, virtual_surface_size)
            }
            ViewportStrategy::FitFloatScaling(virtual_surface_size) => {
                Self::viewport_from_float_scale(self.physical_surface_size, virtual_surface_size)
            }
            ViewportStrategy::MatchPhysicalSize => URect::new(
                0,
                0,
                self.physical_surface_size.x,
                self.physical_surface_size.y,
            ),
        };

        let view_proj_matrix = match self.viewport_strategy {
            ViewportStrategy::MatchPhysicalSize => {
                create_view_uniform_view_projection_matrix(self.physical_surface_size)
            }
            ViewportStrategy::FitFloatScaling(virtual_surface_size)
            | ViewportStrategy::FitIntegerScaling(virtual_surface_size) => {
                create_view_projection_matrix_from_virtual(
                    virtual_surface_size.x,
                    virtual_surface_size.y,
                )
            }
        };

        let scale_matrix = Matrix4::from_scale(self.scale, self.scale, 0.0);
        let origin_translation_matrix =
            Matrix4::from_translation(-self.origin.x as f32, -self.origin.y as f32, 0.0);

        let total_matrix = scale_matrix * view_proj_matrix * origin_translation_matrix;

        // write all model_matrix and uv_coords to instance buffer once, before the render pass
        self.queue.write_buffer(
            &self.camera_buffer,
            0,
            bytemuck::cast_slice(&[total_matrix]),
        );

        self.prepare_render(materials, fonts);

        render_pass.set_viewport(
            self.viewport.position.x as f32,
            self.viewport.position.y as f32,
            self.viewport.size.x as f32,
            self.viewport.size.y as f32,
            0.0,
            1.0,
        );

        render_pass.set_pipeline(&self.pipeline);

        // Index and vertex buffers never change
        render_pass.set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint16);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));

        // Vertex buffer is reused
        render_pass.set_vertex_buffer(1, self.quad_matrix_and_uv_instance_buffer.slice(..));

        // Camera is the same for everything
        render_pass.set_bind_group(0, &self.camera_bind_group, &[]);

        let num_indices = swamp_wgpu_sprites::INDICES.len() as u32;

        for &(weak_material_ref, start, count) in self.batch_offsets.iter() {
            let wgpu_material = materials
                .get_weak(weak_material_ref)
                .expect("no such material");
            // Bind the texture and sampler bind group (Bind Group 1)
            render_pass.set_bind_group(1, &wgpu_material.texture_and_sampler_bind_group, &[]);

            // Issue the instanced draw call for the batch
            trace!(material=%weak_material_ref, start=%start, count=%count, "draw instanced");
            render_pass.draw_indexed(0..num_indices, 0, start..(start + count));
        }

        self.items.clear();
    }

    pub fn material_from_texture(&mut self, texture: wgpu::Texture, label: &str) -> Material {
        trace!("load texture from memory with name: '{label}'");
        let size = &texture.size();
        let texture_and_sampler_bind_group =
            swamp_wgpu_sprites::create_sprite_texture_and_sampler_bind_group(
                &self.device,
                &self.texture_sampler_bind_group_layout,
                texture,
                &self.sampler,
                label,
            );

        let texture_size = UVec2::new(size.width as u16, size.height as u16);

        Material {
            texture_and_sampler_bind_group,
            //render_pipeline: Arc::clone(&self.pipeline),
            texture_size,
        }
    }
}

//fn set_view_projection(&mut self) {}

fn create_view_projection_matrix_from_virtual(virtual_width: u16, virtual_height: u16) -> Matrix4 {
    OrthoInfo {
        left: 0.0,
        right: virtual_width as f32,
        bottom: 0.0,
        top: virtual_height as f32,
        near: 1.0,
        far: -1.0,
    }
    .into()
}

fn create_view_uniform_view_projection_matrix(viewport_size: UVec2) -> Matrix4 {
    let viewport_width = viewport_size.x as f32;
    let viewport_height = viewport_size.y as f32;

    let viewport_aspect_ratio = viewport_width / viewport_height;

    let scale_x = 1.0;
    let scale_y = viewport_aspect_ratio; // scaling Y probably gives the best precision?

    let view_projection_matrix = [
        [scale_x, 0.0, 0.0, 0.0],
        [0.0, scale_y, 0.0, 0.0],
        [0.0, 0.0, -1.0, 0.0],
        [0.0, 0.0, 0.0, 1.0],
    ];

    view_projection_matrix.into()
}

fn sort_render_items_by_z_and_material(items: &mut [RenderItem]) {
    items.sort_by_key(|item| (item.position.z, item.material_ref));
}

#[derive(Default, Debug)]
pub struct SpriteParams {
    pub dest_size: Option<UVec2>,
    pub source: Option<URect>,
    pub rotation: u16,
    pub flip_x: bool,
    pub flip_y: bool,
    pub pivot: Option<Vec2>,
}

#[derive(Debug, PartialEq, Eq, Asset)]
pub struct Material {
    pub texture_and_sampler_bind_group: BindGroup,
    //pub render_pipeline: RenderPipelineRef,
    pub texture_size: UVec2,
}

impl PartialOrd<Self> for Material {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(
            self.texture_and_sampler_bind_group
                .cmp(&other.texture_and_sampler_bind_group),
        )
    }
}

impl Ord for Material {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.texture_and_sampler_bind_group
            .cmp(&other.texture_and_sampler_bind_group)
    }
}

#[derive(Debug)]
pub struct Sprite {
    pub atlas_rect: URect,

    pub params: SpriteParams,
}

#[derive(Debug)]
pub struct TileMap {
    pub tiles_data_grid_size: UVec2,
    pub cell_count_size: UVec2,
    pub one_cell_size: UVec2,
    pub tiles: Vec<u16>,
    pub scale: u8,
}

pub type RenderPipelineRef = Arc<RenderPipeline>;

const fn sources() -> (&'static str, &'static str) {
    let vertex_shader_source = "
// Bind Group 0: Uniforms (view-projection matrix)
struct Uniforms {
    view_proj: mat4x4<f32>,
};

@group(0) @binding(0)
var<uniform> camera_uniforms: Uniforms;

// Bind Group 1: Texture and Sampler (Unused in Vertex Shader but needed for consistency)
@group(1) @binding(0)
var diffuse_texture: texture_2d<f32>;

@group(1) @binding(1)
var sampler_diffuse: sampler;

// Vertex input structure
struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) tex_coords: vec2<f32>,
    @builtin(instance_index) instance_idx: u32,
};

// Vertex output structure to fragment shader
struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
};

// Vertex shader entry point
@vertex
fn vs_main(
    input: VertexInput,
    // Instance attributes
    @location(2) model_matrix0: vec4<f32>,
    @location(3) model_matrix1: vec4<f32>,
    @location(4) model_matrix2: vec4<f32>,
    @location(5) model_matrix3: vec4<f32>,
    @location(6) tex_multiplier: vec4<f32>,
) -> VertexOutput {
    var output: VertexOutput;

    // Reconstruct the model matrix from the instance data
    let model_matrix = mat4x4<f32>(
        model_matrix0,
        model_matrix1,
        model_matrix2,
        model_matrix3,
    );

    // Compute world position
    let world_position = model_matrix * vec4<f32>(input.position, 1.0);

    // Apply view-projection matrix
    output.position = camera_uniforms.view_proj * world_position;

    // Modify texture coordinates
    output.tex_coords = input.tex_coords * tex_multiplier.xy + tex_multiplier.zw;

    return output;
}
        ";
    //

    let fragment_shader_source = "

// Bind Group 1: Texture and Sampler
@group(1) @binding(0)
var diffuse_texture: texture_2d<f32>;

@group(1) @binding(1)
var sampler_diffuse: sampler;

// Fragment input structure from vertex shader
struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
};

// Fragment shader entry point
@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    // Sample the texture using the texture coordinates
    let color = textureSample(diffuse_texture, sampler_diffuse, input.tex_coords);
    return color;
}

";
    (vertex_shader_source, fragment_shader_source)
}
