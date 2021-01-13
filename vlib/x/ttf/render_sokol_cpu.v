module ttf
/**********************************************************************
*
* BMP render module utility functions
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* Note:
*
* TODO: 
**********************************************************************/
import math
import gg
import sokol.sgl

pub
struct TTF_render_Sokol {
pub mut:
	bmp &BitMap // Base bitmap render
	
	// rendering fields
	sg_img C.sg_image // sokol image
	scale_reduct f32  = 2.0 // scale of the cpu texture for filtering
	device_dpi   int  = 72  // device DPI
}

/******************************************************************************
*
* Render functions
*
******************************************************************************/
fn (mut tf_skl TTF_render_Sokol) format_texture(){
	r := byte(tf_skl.bmp.color >> 24)
	g := byte((tf_skl.bmp.color >> 16) & 0xFF)
	b := byte((tf_skl.bmp.color >> 8 ) & 0xFF)
	a := byte(tf_skl.bmp.color & 0xFF)

	b_r := byte(tf_skl.bmp.bg_color >> 24)
	b_g := byte((tf_skl.bmp.bg_color >> 16) & 0xFF)
	b_b := byte((tf_skl.bmp.bg_color >> 8 ) & 0xFF)
	b_a := byte(tf_skl.bmp.bg_color & 0xFF)
	
	// trasform buffer in a texture
	x := byteptr(tf_skl.bmp.buf)
	unsafe{
		mut i := 0
		for i<tf_skl.bmp.buf_size {
			data := x[i]
			if data > 0 {			
				x[i+0] = r
				x[i+1] = g
				x[i+2] = b
				// alpha
				x[i+3] = byte((a * data) >> 8)
			} else {
				x[i+0] = b_r
				x[i+1] = b_g
				x[i+2] = b_b
				x[i+3] = b_a
			}
			i += 4
		}
	}
}

pub
fn (mut tf_skl TTF_render_Sokol) create_text(in_txt string, in_font_size f32){
	scale_reduct := tf_skl.scale_reduct
	device_dpi := tf_skl.device_dpi
	font_size := in_font_size //* scale_reduct
	
	// Formula: (font_size * device dpi) / (72dpi * em_unit)
	//scale := ((1.0  * devide_dpi )/ f32(72 * tf_skl.bmp.tf.units_per_em))* font_size
	scale := f32(font_size * device_dpi) / f32(72 * tf_skl.bmp.tf.units_per_em)
	//dprintln("Scale: $scale")
		
	tf_skl.bmp.scale =  scale * scale_reduct
	w, h := tf_skl.bmp.get_bbox(in_txt)
	tf_skl.bmp.width  = int(w)
	tf_skl.bmp.height = int((h+8))
	sz := tf_skl.bmp.width * tf_skl.bmp.height * tf_skl.bmp.bp
	
	// RAM buffer
	if sz > tf_skl.bmp.buf_size {
		if sz > 0 {
			free(tf_skl.bmp.buf)
		}
		dprintln("create_text Alloc: $sz bytes")
		tf_skl.bmp.buf = malloc(sz)
		tf_skl.bmp.buf_size = sz
	}

	tf_skl.bmp.init_filler()
	
	// draw the text
	mut y_base := int((tf_skl.bmp.tf.y_max - tf_skl.bmp.tf.y_min) * tf_skl.bmp.scale)
	tf_skl.bmp.set_pos(0,y_base)
	tf_skl.bmp.clear()
	tf_skl.bmp.draw_text(in_txt)
	tf_skl.format_texture()
}

pub
fn (mut tf_skl TTF_render_Sokol) create_text_block(in_txt string, in_w int, in_h int, in_font_size f32){
	scale_reduct := tf_skl.scale_reduct
	device_dpi := tf_skl.device_dpi
	font_size := in_font_size //* scale_reduct
	// Formula: (font_size * device dpi) / (72dpi * em_unit)
	//scale := ((1.0  * devide_dpi )/ f32(72 * tf_skl.bmp.tf.units_per_em))* font_size
	scale := f32(font_size * device_dpi) / f32(72 * tf_skl.bmp.tf.units_per_em)
	//dprintln("Scale: $scale")
		
	tf_skl.bmp.scale =  scale * scale_reduct
	w := in_w
	h := in_h
	tf_skl.bmp.width  = int(w * scale_reduct + 0.5)
	tf_skl.bmp.height = int((h+2) * scale_reduct + 0.5)
	sz := tf_skl.bmp.width * tf_skl.bmp.height * tf_skl.bmp.bp
		
	//if true { return }
	
	// RAM buffer
	if sz > tf_skl.bmp.buf_size {
		if sz > 0 {
			free(tf_skl.bmp.buf)
		}
		dprintln("Alloc: $sz bytes")
		tf_skl.bmp.buf = malloc(sz)
		tf_skl.bmp.buf_size = sz
	}

	tf_skl.bmp.init_filler()
	
	// draw the text
	mut y_base := int((tf_skl.bmp.tf.y_max - tf_skl.bmp.tf.y_min) * tf_skl.bmp.scale)
	tf_skl.bmp.set_pos(0,y_base)
	tf_skl.bmp.clear()
	
	tf_skl.bmp.draw_text_block(in_txt, {x: 0, y:0, w:w, h:h})
	tf_skl.format_texture()
}
/******************************************************************************
*
* Sokol Render functions
*
******************************************************************************/
pub
fn (mut tf_skl TTF_render_Sokol) create_texture(){
	w := tf_skl.bmp.width
	h := tf_skl.bmp.height
	sz := tf_skl.bmp.width * tf_skl.bmp.height * tf_skl.bmp.bp
	mut img_desc := C.sg_image_desc{
		width: w
		height: h
		num_mipmaps: 0
		min_filter:   .linear
		mag_filter:   .linear
		//usage: .dynamic
		wrap_u: .clamp_to_edge
		wrap_v: .clamp_to_edge
		label: &byte(0)
		d3d11_texture: 0
	}
	// comment for dynamic
	img_desc.content.subimage[0][0] = C.sg_subimage_content{
		ptr: tf_skl.bmp.buf
		size: sz
	}
	
	simg := C.sg_make_image(&img_desc)
	//free(tf_skl.bmp.buf)  // DONT FREE IF Dynamic
	tf_skl.sg_img = simg
}

pub
fn (tf_skl TTF_render_Sokol) destroy_texture(){
	C.sg_destroy_image(tf_skl.sg_img)
}

// Use only if usage: .dynamic
pub
fn (mut tf_skl TTF_render_Sokol) update_text_texture(){
	sz := tf_skl.bmp.width * tf_skl.bmp.height * tf_skl.bmp.bp
	mut tmp_sbc := C.sg_image_content{}
	tmp_sbc.subimage[0][0] = C.sg_subimage_content {
		ptr: tf_skl.bmp.buf
		size: sz
	}
	C.sg_update_image(tf_skl.sg_img, &tmp_sbc)
}

pub
fn (tf_skl TTF_render_Sokol) draw_text_bmp(ctx &gg.Context, x f32, y f32) {
	//width  := tf_skl.bmp.width  >> 1
	//height := tf_skl.bmp.height >> 1
	sgl.push_matrix()
	
	width  := tf_skl.bmp.width  / (tf_skl.scale_reduct)
	height := tf_skl.bmp.height / (tf_skl.scale_reduct)
	
	u0 := f32(0.0)
	v0 := f32(0.0)
	u1 := f32(1.0)
	v1 := f32(1.0)
	x0 := f32(0)
	y0 := f32(0)
	x1 := f32(width) * ctx.scale
	y1 := f32(height) * ctx.scale

	ca := f32(math.cos(tf_skl.bmp.angle))
	sa := f32(math.sin(tf_skl.bmp.angle))
	m := [
		f32(ca),-sa,0,0,
		sa,ca,0,0,
		0,0,1,0,
		x,y,0,1
	]
	sgl.mult_matrix(m)
	//
	sgl.load_pipeline(ctx.timage_pip)
	sgl.enable_texture()
	sgl.texture(tf_skl.sg_img)
	sgl.begin_quads()
	sgl.c4b(255, 255, 255, 255)
	sgl.v2f_t2f(x0, y0, u0, v0)
	sgl.v2f_t2f(x1, y0, u1, v0)
	sgl.v2f_t2f(x1, y1, u1, v1)
	sgl.v2f_t2f(x0, y1, u0, v1)
	sgl.end()
	sgl.disable_texture()
	sgl.pop_matrix()
}
