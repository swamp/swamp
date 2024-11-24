/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use int_math::{URect, UVec2, Vec2};
use limnus::prelude::*;
use monotonic_time_rs::{InstantMonotonicClock, Millis, MonotonicClock};
use std::cmp::{max, min};
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use swamp_font::Font;
use swamp_game_assets::{Assets, GameAssets};
use swamp_game_audio::{Audio, GameAudio};
use swamp_render_wgpu::{Gfx, Material, Render};
use tracing::debug;

pub trait Application: Send + Sync + Sized + 'static {
    fn new(assets: &mut impl Assets) -> Self;
    fn tick(&mut self, assets: &mut impl Assets);
    fn render(&mut self, gfx: &mut impl Gfx);
    fn audio(&mut self, _audio: &mut impl Audio) {}

    fn wants_to_quit(&self) -> bool {
        false
    }

    fn wants_cursor_visible(&self) -> bool {
        true
    }

    fn keyboard_input(&mut self, _state: ButtonState, _key_code: KeyCode) {}

    fn cursor_entered(&mut self) {}

    fn cursor_left(&mut self) {}

    fn cursor_moved(&mut self, _position: UVec2) {}

    fn mouse_input(&mut self, _state: ButtonState, _button: MouseButton) {}

    fn mouse_wheel(&mut self, _delta_y: i16) {}

    fn mouse_motion(&mut self, _delta: Vec2) {}

    fn scale_factor_changed(&mut self, _scale_factor: f64) -> Option<UVec2> {
        None
    }
}

#[derive(Debug, Resource)]
pub struct GameSettings {
    pub virtual_size: UVec2,
}

#[derive(Resource)]
pub struct Game<G: Application> {
    game: G,
    clock: InstantMonotonicClock,
}

impl<G: Application> Debug for Game<G> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "WgpuGame")
    }
}

impl<G: Application> Game<G> {
    #[must_use]
    pub fn new(all_resources: &mut ResourceStorage) -> Self {
        let clock = InstantMonotonicClock::new();
        let mut assets = GameAssets::new(all_resources, clock.now());
        let game = G::new(&mut assets);

        Self { game, clock }
    }

    pub fn inputs(&mut self, iter: MessagesIterator<InputMessage>) {
        for message in iter {
            match message {
                InputMessage::KeyboardInput(button_state, key_code) => {
                    self.game.keyboard_input(*button_state, *key_code)
                }
                InputMessage::MouseInput(button_state, button) => {
                    self.game.mouse_input(*button_state, *button);
                }
                InputMessage::MouseWheel(scroll_delta, _touch_phase) => {
                    if let MouseScrollDelta::LineDelta(delta) = scroll_delta {
                        let game_scroll_y = (-delta.y as f32 * 120.0) as i16;
                        self.game.mouse_wheel(game_scroll_y);
                    }
                }
            }
        }
    }

    pub fn cursor_moved(
        &mut self,
        physical_position: UVec2,
        viewport: URect,
        virtual_surface_size: UVec2,
    ) {
        let relative_x = max(
            0,
            min(
                physical_position.x as i64 - viewport.position.x as i64,
                (viewport.size.x - 1) as i64,
            ),
        );

        let relative_y = max(
            0,
            min(
                physical_position.y as i64 - viewport.position.y as i64,
                (viewport.size.y - 1) as i64,
            ),
        );

        let clamped_to_viewport: UVec2 = UVec2::new(relative_x as u16, relative_y as u16);

        let virtual_position_x =
            (clamped_to_viewport.x as u64 * virtual_surface_size.x as u64) / viewport.size.x as u64;

        let virtual_position_y =
            (clamped_to_viewport.y as u64 * virtual_surface_size.y as u64) / viewport.size.y as u64;

        let virtual_position = UVec2::new(virtual_position_x as u16, virtual_position_y as u16);
        self.game.cursor_moved(virtual_position)
    }

    pub fn mouse_move(&mut self, iter: MessagesIterator<WindowMessage>, wgpu_render: &Render) {
        for message in iter {
            match message {
                WindowMessage::CursorMoved(position) => self.cursor_moved(
                    *position,
                    wgpu_render.viewport(),
                    wgpu_render.virtual_surface_size(),
                ),
                WindowMessage::WindowCreated() => {}
                WindowMessage::Resized(_) => {}
            }
        }
    }

    pub fn tick(&mut self, storage: &mut ResourceStorage, now: Millis) {
        // This is a quick operation, we basically wrap storage
        let mut assets = GameAssets::new(storage, now);

        self.game.tick(&mut assets);
    }

    pub fn render(
        &mut self,
        wgpu: &WgpuWindow,
        wgpu_render: &mut Render,
        materials: &limnus::prelude::Assets<Material>,
        fonts: &limnus::prelude::Assets<Font>,
        now: Millis,
    ) {
        wgpu_render.set_now(now);
        self.game.render(wgpu_render);

        wgpu.render(wgpu_render.clear_color(), |render_pass| {
            wgpu_render.render(render_pass, materials, fonts, now)
        })
        .unwrap();
    }
}

pub struct GamePlugin<G: Application> {
    pub phantom_data: PhantomData<G>,
}
impl<G: Application> Default for GamePlugin<G> {
    fn default() -> Self {
        Self::new()
    }
}

impl<G: Application> GamePlugin<G> {
    pub const fn new() -> Self {
        Self {
            phantom_data: PhantomData,
        }
    }
}

// TODO: add support for having tuple arguments to have maximum seven parameters
#[allow(clippy::too_many_arguments)]
pub fn tick<G: Application>(
    window: Re<WgpuWindow>,
    mut wgpu_render: ReM<Render>,

    input_messages: Msg<InputMessage>,
    window_messages: Msg<WindowMessage>,
    mut all_resources: ReAll,
    mut all_local_resources: LocReAll,
    mut internal_game: ReM<Game<G>>,
) {
    let now = internal_game.clock.now();

    // Inputs
    {
        internal_game.inputs(input_messages.iter_previous());
        internal_game.mouse_move(window_messages.iter_previous(), &wgpu_render);
    }

    // Tick
    {
        internal_game.tick(&mut all_resources, now);
        if internal_game.game.wants_to_quit() {
            all_resources.insert(ApplicationExit {
                value: AppReturnValue::Value(0),
            });
        }
    }

    // Audio
    {
        let samples = all_resources.fetch::<limnus::prelude::Assets<StereoSample>>();
        let mixer = all_local_resources.fetch_mut::<AudioMixer>();
        let mut game_audio = GameAudio::new(mixer, samples);
        internal_game.game.audio(&mut game_audio);
    }

    // Render
    {
        let materials = all_resources.fetch::<limnus::prelude::Assets<Material>>();
        let fonts = all_resources.fetch::<limnus::prelude::Assets<Font>>();
        internal_game.render(&window, &mut wgpu_render, materials, fonts, now);
    }
}

impl<G: Application> Plugin for GamePlugin<G> {
    fn post_initialization(&self, app: &mut App) {
        debug!("calling WgpuGame::new()");
        let all_resources = app.resources_mut();
        let internal_game = Game::<G>::new(all_resources);
        app.insert_resource(internal_game);

        app.add_system(UpdatePhase::Update, tick::<G>);
    }
}

/*
pub trait Gfx {
    // Physical surface and viewport
    fn physical_aspect_ratio(&self) -> AspectRatio;
    fn physical_size(&self) -> UVec2;
    fn set_viewport(&mut self, viewport_strategy: ViewportStrategy);
    fn viewport(&self) -> &ViewportStrategy;

    // "Camera" (Project and view matrix)
    fn set_scale(&mut self, scale_factor: VirtualScale);
    fn set_origin(&mut self, position: Vec2);

    // Other
    fn set_clear_color(&mut self, color: Color);

    // Sprite
    fn sprite_atlas_frame(&mut self, position: Vec3, frame: u16, atlas: &impl FrameLookup);
    fn sprite_atlas(&mut self, position: Vec3, atlas_rect: URect, material: &MaterialRef);

    // Text
    fn text_draw(&mut self, position: Vec3, text: &str, font_ref: &FontAndMaterialRef);
    fn text_glyphs(&self, position: Vec2, text: &str, font_ref: &FontAndMaterialRef) -> Vec<Glyph>;

    // Tilemap
    fn tilemap(&mut self, position: Vec3, tiles: &[u16], width: u16, atlas: &FixedAtlas);
    fn tilemap_params(
        &mut self,
        position: Vec3,
        tiles: &[u16],
        width: u16,
        atlas: &FixedAtlas,
        scale: u8,
    );
    fn now(&self) -> Millis;
}
*/
