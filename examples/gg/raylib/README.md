# raylib examples ported to gg

This directory contains V ports of the [raylib](https://github.com/raysan5/raylib)
example suite, written against gg's API. The ports live on their own branch so
the gg feature work in PR #32612 stays focused.

## Layout

- `core/` — window, input, camera, timing, viewport and platform examples
- `shapes/` — 2D primitives, collision, easing and visual effects
- `text/` — fonts, formatting, text animations and sprite fonts
- `textures/` — images, sprites, blending and render targets
- `models/` — 3D meshes, models, materials, picking and cameras
- `shaders/` — approximations of raylib's shader examples with gg materials,
  render targets and immediate-mode geometry
- `audio/` — WAV/PCM sound playback through gg's small sokol.audio wrapper

## Naming

Each file is `category_example_name.v`, matching the upstream raylib name, and
is a standalone `module main` program. Run one with:

```sh
v run examples/gg/raylib/core/core_basic_window.v
```

## Porting notes

Not every raylib example maps one-to-one onto gg. Look for `// PORT NOTE:`
comments in the source: they mark where a feature is unavailable and describe
the closest gg behaviour used instead. Known gaps:

- **Shaders** — gg has no user-supplied shader API yet; shader examples are
  approximated with `Material3D`, `RenderTexture2D` and immediate-mode draws.
- **Animated models** — glTF skin/animation data is not loaded by `load_model`.
- **Alternate model formats** — IQM, VOX and M3D loaders are not in gg.
- **Gamepad / VR** — no gg API; those examples demonstrate the closest input
  or 3D behaviour available.
- **GIF, SDF text, colour emoji** — no decoder/atlas support in gg.

These gaps are tracked in the gg 3D roadmap; examples are updated as the
corresponding APIs land.
