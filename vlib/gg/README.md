## Description

`gg` is V's simple graphics module.
It is currently implemented using `sokol`, and makes easy creating
apps that just need a way to draw simple 2D shapes, and to react to
user's keyboard/mouse input.

## Example

```v cgen
module main

import gg
import gx

fn main() {
	mut context := gg.new_context(
		bg_color:     gx.rgb(174, 198, 255)
		width:        600
		height:       400
		window_title: 'Polygons'
		frame_fn:     frame
	)
	context.run()
}

fn frame(mut ctx gg.Context) {
	ctx.begin()
	ctx.draw_convex_poly([f32(100.0), 100.0, 200.0, 100.0, 300.0, 200.0, 200.0, 300.0, 100.0, 300.0],
		gx.blue)
	ctx.draw_poly_empty([f32(50.0), 50.0, 70.0, 60.0, 90.0, 80.0, 70.0, 110.0], gx.black)
	ctx.draw_triangle_filled(450, 142, 530, 280, 370, 280, gx.red)
	ctx.end()
}
```

## Troubleshooting

A common problem, if you draw a lot of primitive elements in the same
frame, is that there is a chance that your program can exceed the maximum
allowed amount of vertices and commands, imposed by `sokol`.
The symptom is that your frame will be suddenly black, after it becomes more complex.
Sokol's default for vertices is 131072.
Sokol's default for commands is 32768.

To solve that, you can try adding these lines at the top of your program:
`#flag -D_SGL_DEFAULT_MAX_VERTICES=4194304`
`#flag -D_SGL_DEFAULT_MAX_COMMANDS=65536`
You can see an example of that in:
https://github.com/vlang/v/blob/master/examples/gg/many_thousands_of_circles_overriding_max_vertices.v

Another approach is to use several draw passes, and limit the amount
of draw calls that you make in each, demonstrated in:
https://github.com/vlang/v/blob/master/examples/gg/many_thousands_of_circles.v

Another approach to that problem, is to draw everything yourself in a streaming
texture, then upload that streaming texture as a single draw command to the GPU.
You can see an example of that done in:
https://github.com/vlang/v/blob/master/examples/gg/random.v

A third approach, is to only upload your changing inputs to the GPU, and do all
the calculations and drawing there in shaders.
