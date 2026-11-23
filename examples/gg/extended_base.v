module main

// Extended helper starter: physics, splats, textures, XR rig, GPU material
// slots and app composition. Run with: v run examples/gg/extended_base.v
import gg
import gg.gpu
import gg.physics
import gg.splat
import gg.texture
import gg.xr
import gg.scene
import gg.fx
import time

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	world    physics.World
	cloud    splat.SplatCloud
	rig      xr.StereoRig
	material gpu.ShaderMaterial
	app      scene.App
	theme    fx.UITheme
	last     time.Time
}

fn main() {
	mut camera := gg.Camera3D{}
	camera.position = gg.Vec3{0, 1, 6}
	camera.target = gg.Vec3{0, 1, 0}
	camera.fovy = 60
	camera.near = 0.1
	camera.far = 100

	mut state := &App{
		cloud:    splat.new_cloud()
		rig:      xr.stereo_from_camera(camera, 0.064)
		material: gpu.new_material('tint')
		theme:    fx.default_ui_theme()
		app:      scene.new_app()
	}
	state.world.floor_y = -1
	ball := physics.new_body(1, gg.Vec3{0, 3, 0}, 0.6)
	state.world.add(ball)
	state.cloud.points << splat.Gaussian{gg.Vec3{0, 1, 0}, gg.Vec3{0.5, 0.5, 0.5}, gg.rgba(255, 160, 80, 255), 0.8}
	state.material.set_color('tint', gg.rgba(200, 120, 255, 255))
	state.material.set_float('time', 0)
	state.last = time.now()

	state.ctx = gg.new_context(
		width:        640
		height:       400
		window_title: 'gg extended base'
		bg_color:     gg.rgb(18, 20, 28)
		frame_fn:     frame
		user_data:    state
	)
	state.ctx.run()
}

fn frame(mut state App) {
	now := time.now()
	mut dt := f32((now - state.last).seconds())
	state.last = now
	if dt > 0.1 {
		dt = 0.1
	}
	state.world.step(dt)
	state.material.set_float('time', state.material.uniform_block()[0] + dt)
	state.app.update(dt)

	mut ball_min := gg.Vec3{}
	mut ball_max := gg.Vec3{}
	for body in state.world.bodies {
		ball_min = body.position - gg.Vec3{body.radius, body.radius, body.radius}
		ball_max = body.position + gg.Vec3{body.radius, body.radius, body.radius}
	}

	left, right := xr.viewports(640, 200)
	state.ctx.begin()
	state.ctx.draw_text_def(10, 10, 'extended helpers: physics / splats / gpu slots / xr')
	tiles_x, tiles_y := texture.block_dimensions(64, 64)
	state.ctx.draw_rect_filled(10, 30, f32(tiles_x * tiles_y), 4, state.theme.accent)
	state.ctx.draw_rect_filled(left.x, left.y, left.width, 2, state.theme.accent)
	state.ctx.draw_rect_filled(right.x, right.y, right.width, 2, state.theme.accent)
	state.ctx.draw_circle_filled(ball_max.x * 40 + 320, 120 - (ball_min.y - ball_max.y) * 30, 14,
		gg.rgb(255, 180, 90))
	state.ctx.end()
}
