# TTF font utility

## introduction

This module is designed to perform two main task

- Load the font file
- Render text using a TTF font

The render system can be single or multiple, for example it is possible to have a bitmap
render and a HW accelerated render.

## TTF loader

This part of the module do a simple task, load a TTF file and preprocess all the loaded data
in order to simplify the rendering phase.

Let's start with a simple snippet of code that load a font from the disk:

```v ignore
mut ttf_font := ttf.TTF_File{}
ttf_font.buf = os.read_bytes("arial.ttf") or { panic(err) }
ttf_font.init()
```

> **Note**
> The font must be passed to the `TTF_file` as RAM buffer.

At this point the font "arial" is loaded and parsed and if it is a valid TTF font it is
ready for the rendering.
We can get some quick info on the font as string using the `get_info_string` function:

```v oksyntax
println(ttf_font.get_info_string())
```

produces an output like this:

```
----- Font Info -----
font_family     : Arial
font_sub_family : Normal
full_name       : Arial
postscript_name : ArialMT
version         : 1
font_revision   : 5.06
magic_number    : 5f0f3cf5
flags           : 81b
created  unixTS : 649950890
modified unixTS : 1282151447
units_per_em    : 2048
box             : [x_min:-1361, y_min:-665, x_Max:4096, y_Max:2060]
mac_style       : 0
-----------------------
```

Once loaded a font the `TTF_File` struct is filled with the font data and texts can be rendered.
At high level no more action are required to use the loaded font.
Multiple fonts can be loaded without problems at the same time.

## TTF Bitmap render

In this module it is possible to have different renders running at the same time.
At the present time all the rendering are made on the CPU, sokol is used only to draw the
rendered text to the screen.
Let's start with a simple snippet of code:

```v oksyntax
import os
import x.ttf

fn main() {
	mut ttf_font := ttf.TTF_File{}
	ttf_font.buf = os.read_bytes('arial.ttf') or { panic(err) }
	ttf_font.init()
	// print font info
	println(ttf_font.get_info_string())
}
```

This simple code load a TTF font and display its basic information.

### draw_text

The draw text function draw simple strings without indentation or other imagination tasks.
At this point we can render a simple text:

```v oksyntax
import os
import x.ttf

fn main() {
	mut ttf_font := ttf.TTF_File{}
	ttf_font.buf = os.read_bytes('arial.ttf') or { panic(err) }
	ttf_font.init()
	// print font info
	println(ttf_font.get_info_string())

	bmp_width := 200
	bmp_height := 64
	bmp_layers := 4 // number of planes for an RGBA buffer
	// memory size of the buffer
	bmp_size := bmp_width * bmp_height * bmp_layers

	font_size := 32 // font size in points
	device_dpi := 72 // default screen DPI
	// Formula for scale calculation
	// scaler := (font_size * device dpi) / (72dpi * em_unit)
	scale := f32(font_size * device_dpi) / f32(72 * ttf_font.units_per_em)
	// height of the font to use in the buffer to separate the lines
	y_base := int((ttf_font.y_max - ttf_font.y_min) * scale)

	// declare the bitmap struct
	mut bmp := ttf.BitMap{
		tf:       &ttf_font
		buf:      malloc(bmp_size)
		buf_size: bmp_size
		width:    bmp_width
		height:   bmp_height
		bp:       bmp_layers
		color:    0x000000_FF // RGBA black
		scale:    scale
	}
	bmp.init_filler()
	bmp.clear()
	bmp.set_pos(10, y_base)
	bmp.draw_text('Test Text!')
	bmp.save_as_ppm('test.ppm')
}
```

This is the low level render that draw the text on a bitmap and save the bitmap on a disk as
`.ppm` file.

> **Note**
> The render in this case is a raw rendering without any postfiltering or other processing.

Using the low level rendering you need to manage all the amenities like allocate and release
memory and other tasks like calc the character dimensions.

You can specify the style for the text rendering in the `BitMap` struct::

```v
enum Style {
	outline
	outline_aliased
	filled // default style
	raw
}
```

Use this level only if you want achieve particular result on text rendering.

### draw_text_block

Draw text block draw a justified and indented block of multiline text in the bitmap.

```v oksyntax
import os
import x.ttf

fn main() {
	mut ttf_font := ttf.TTF_File{}
	ttf_font.buf = os.read_bytes('arial.ttf') or { panic(err) }
	ttf_font.init()
	// print font info
	println(ttf_font.get_info_string())

	bmp_width := 200
	bmp_height := 200
	bmp_layers := 4 // number of planes for an RGBA buffer
	// memory size of the buffer
	bmp_size := bmp_width * bmp_height * bmp_layers

	font_size := 32 // font size in points
	device_dpi := 72 // default screen DPI
	// Formula for scale calculation
	// scaler := (font_size * device dpi) / (72dpi * em_unit)
	scale := f32(font_size * device_dpi) / f32(72 * ttf_font.units_per_em)
	// height of the font to use in the buffer to separate the lines
	y_base := int((ttf_font.y_max - ttf_font.y_min) * scale)

	text := "Today it is a good day!
Tomorrow I'm not so sure :(
But Vwill prevail for sure, V is the way!!
òàèì@ò!£$%&
"
	// declare the bitmap struct
	mut bmp := ttf.BitMap{
		tf:       &ttf_font
		buf:      malloc(bmp_size)
		buf_size: bmp_size
		width:    bmp_width
		height:   bmp_height
		bp:       bmp_layers
		color:    0x000000_FF // RGBA black
		scale:    scale
	}
	bmp.init_filler()
	bmp.clear()
	bmp.justify = true
	bmp.align = .left
	bmp.draw_text_block(text, x: 0, y: 0, w: bmp_width - 20, h: bmp_height)
	bmp.save_as_ppm('test.ppm')
}
```

This is the low level render that draw text block on the bitmap.
A text block is defined from a `Text_block` struct:

```v
struct Text_block {
	x         int // x position of the left high corner
	y         int // y position of the left high corner
	w         int // width of the text block
	h         int // height of the text block
	cut_lines bool = true // force to cut the line if the length is over the text block width
}
```

and use the following bitmap fields:

```v ignore
	style              Style      = .filled // default style
	align              Text_align = .left   // default text align
	justify            bool				    // justify text flag, default deactivated
	justify_fill_ratio f32        = 0.5     // justify fill ratio, if the ratio of the filled
	                                        // row is >= of this then justify the text
```

It is possible to modify these parameters to obtain the desired effect on the text rendering.

## TTF Sokol render

The sokol render use the bitmap render to create the text and the `gg` functions to render
the text to the screen.
It is simpler to use in a `gg app` than the raw bitmap render.
Each single text rendered need its own reder to be declared, after you can modify it.
Here a simple example of the usage:

```v oksyntax
import gg
import gx
import sokol.sapp
import sokol.sgl
import sokol.gfx
import x.ttf
import os

const win_width = 600
const win_height = 700
const bg_color = gx.white
const font_paths = [
	'arial.ttf',
]

struct App_data {
pub mut:
	gg        &gg.Context = unsafe { nil }
	sg_img    gfx.Image
	init_flag bool
	frame_c   int

	tf         []ttf.TTF_File
	ttf_render []ttf.TTF_render_Sokol
}

fn my_init(mut app App_data) {
	app.init_flag = true
}

fn draw_frame(mut app App_data) {
	cframe_txt := 'Current Frame: ${app.frame_c}'

	app.gg.begin()

	sgl.defaults()
	sgl.matrix_mode_projection()
	sgl.ortho(0.0, f32(sapp.width()), f32(sapp.height()), 0.0, -1.0, 1.0)

	// draw text only if the app is already initialized
	if app.init_flag == true {
		// update the text
		mut txt1 := &app.ttf_render[0]
		txt1.destroy_texture()
		txt1.create_text(cframe_txt, 43)
		txt1.create_texture()
		txt1.draw_text_bmp(app.gg, 30, 60)
	}
	app.frame_c++
	app.gg.end()
}

fn main() {
	mut app := &App_data{}

	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		create_window: true
		window_title:  'Test TTF module'
		user_data:     app
		bg_color:      bg_color
		frame_fn:      draw_frame
		init_fn:       my_init
	)

	// load TTF fonts
	for font_path in font_paths {
		mut tf := ttf.TTF_File{}
		tf.buf = os.read_bytes(font_path) or { panic(err) }
		println('TrueTypeFont file [${font_path}] len: ${tf.buf.len}')
		tf.init()
		println(tf.get_info_string())
		app.tf << tf
	}

	// TTF render 0 Frame counter
	app.ttf_render << &ttf.TTF_render_Sokol{
		bmp: &ttf.BitMap{
			tf:       &app.tf[0]
			buf:      unsafe { malloc(32000000) }
			buf_size: (32000000)
			color:    0xFF0000FF
			// style: .raw
		}
	}

	app.gg.run()
}
```