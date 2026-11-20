# gg/render

V implementation of sokol-gl for gg.

Set `Desc.color_format`, `Desc.depth_format`, and `Desc.sample_count` to match the render pass.
Use `.none` as the depth format for a target without a depth attachment. Explicit context formats
apply to both the default pipeline and pipelines created from a caller's recipe; depth writes are
disabled for a target without depth. Unspecified formats keep the existing graphics defaults and
recipe formats. A zero sample count falls back to one.

Call `Context.destroy()` before graphics shutdown to release its vertex buffer,
default pipelines, image, sampler, shader, and commit listener. Repeated calls are safe.
Pipelines returned by `Context.make_pipeline()` remain owned by the caller; release
them with `Context.destroy_pipeline()` before destroying their shaders.

`Context.load_pipeline()` selects the pipeline for subsequent batches until another pipeline is
loaded or a saved selection is restored with `Context.pop_pipeline()`. `Context.current_pipeline()`
returns that selection so callers can save and restore it without consuming pipeline stack space.
Ending a primitive batch preserves the selected pipeline.

`Context.num_vertices()` and `Context.num_commands()` report the current recording queue.
Vertex counts include the extra vertices used to turn quads into triangles. Command counts
include viewport and scissor commands; compatible consecutive draws can share one command.
Drawing or rendering on the CPU leaves the queue intact. A graphics commit clears the queue.

Use `Context.rewind()` to discard an abandoned recording before calling `draw()`, without
committing a graphics frame. It clears that context's vertices, commands, uniforms and error,
ends any unfinished primitive, and resets the layer to zero. It preserves matrices and the
selected color, texture and pipeline. Other contexts and the frame index are unchanged.
Record the replacement geometry before starting its render pass.

## CPU rendering

`Context.render_cpu()` rasterizes the recorded commands into a `Frame` without a GPU;
it uses painter-order compositing. `Context.render_cpu_depth()` adds a depth buffer
with a per-pixel `LESS_EQUAL` test, near/far clipping and perspective-correct texture
coordinates, so 3D geometry renders correctly in software. `Frame.depth` holds the
depth value per pixel; `Frame.save_ppm()` writes the RGB pixels to a PPM file.

Triangles with any vertex at or behind the near plane are skipped rather than clipped,
and backfaces are not culled on the CPU (depth testing makes the nearest surface win).
`Context.render_cpu_depth_with_mvp()` overrides the model-view-projection matrix stored
in each recorded command, so the camera need not be active while geometry is recorded.
Both CPU paths leave the command queue intact.
