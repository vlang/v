# gg/render

V implementation of sokol-gl for gg.

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
