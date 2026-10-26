# gg/render

V implementation of sokol-gl for gg.

`Context.num_vertices()` and `Context.num_commands()` report the current recording queue.
Vertex counts include the extra vertices used to turn quads into triangles. Command counts
include viewport and scissor commands; compatible consecutive draws can share one command.
Drawing or rendering on the CPU leaves the queue intact. A graphics commit clears the queue.

Use `Context.rewind()` to discard an abandoned recording before calling `draw()`, without
committing a graphics frame. It clears that context's vertices, commands, uniforms and error,
ends any unfinished primitive, and resets the layer to zero. It preserves matrices and the
selected color, texture and pipeline. Other contexts and the frame index are unchanged.
Record the replacement geometry before starting its render pass.
