# gg helper cheatsheet

One-line reference for the opt-in addon API. See `README.md` for detail.

```v ignore
import gg
import gg.anim
import gg.game
import gg.fx
import gg.debug
import gg.loaders
import gg.scene
```

## Core

```v ignore
ctx := gg.new_context(width: 800, height: 600, frame_fn: frame)
ctx.run()
gg.backend_caps().backend      // 'native' or 'js'
mut info := gg.Info{}
info.record_draw(12)
```

## Animation (`gg.anim`)

```v ignore
e := anim.ease(0.5, .ease_in_out_cubic)
mut t := anim.new_tween(f32(0), f32(1), 1, anim.lerp_f32)
t.update(dt)
v := t.value()
n := anim.new_noise(42)
h := n.fbm2d(x, y, 4, 2.0, 0.5)
p := anim.sample_catmull(points, t01)
q := anim.quat_slerp(a, b, t01)
mut mixer := anim.new_mixer()
mixer.add(anim.new_action(clip))
mixer.update(dt)
poses := mixer.sample(joints)
```

## Game (`gg.game`)

```v ignore
overlap := game.rects_overlap(a, b)
push := game.rect_mtv(a, b)
mut input := game.new_input_state()
input.bind('left', [gg.KeyCode.left])
input.on_key_down(gg.KeyCode.left)
down := input.action_down('left')
mut step := game.new_fixed_step(1.0 / 60)
n := step.advance(dt)
mut map := game.new_tilemap(tiles, cols, rows, 32)
solid := map.collides_rect(r)
path := game.grid_from_tilemap(map).find_path(start, goal)
hull := game.convex_hull(points)
maze := game.generate_maze(21, 11, seed)
```

## FX (`gg.fx`)

```v ignore
mut emitter := fx.new_emitter(fx.ParticleConfig{rate: 30})
emitter.update(dt)
emitter.draw(ctx)
gray := fx.post_grayscale(pixels, w, h)
blur := fx.post_box_blur(pixels, w, h, 2)
theme := fx.default_ui_theme()
fx.ui_panel(ctx, panel, theme)
clicked := fx.ui_button(ctx, r, 'Go', mouse, pressed, theme)
fx.draw_line_chart(ctx, values, rect, theme.accent)
mut voice := fx.new_voice(&sound)
voice.play()
fx.mix_voices(mut buffer, frames, channels, [voice])
```

## Debug, loaders, scene

```v ignore
mut log := debug.new_debug_log()
log.add('hello')
mut graph := debug.FrameGraph{}
graph.push(dt)
mut gizmos := debug.new_gizmos()
gizmos.line(a, b, gg.red)
gizmos.draw(ctx)
mut store := debug.new_save_store()
store.set_int('level', 2)
store.save('save.json')
meshes := loaders.load_mesh_glb('model.glb')!
mut manager := loaders.new_loading_manager()
manager.begin(2)
manager.progress('model.glb')
mut root := scene.new_node('root')
root.add(mut child)
mut sig := scene.new_signal[string]()
sig.listen(fn (s string) {})
sig.emit('event')
```
