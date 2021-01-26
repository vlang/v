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
pub struct Text_block {
	x         int  // x postion of the left high corner
	y         int  // y postion of the left high corner
	w         int  // width of the text block
	h         int  // heigth of the text block
	cut_lines bool = true // force to cut the line if the length is over the text block width
}

fn (mut dev BitMap) get_justify_space_cw(txt string, w int, block_w int, space_cw int) f32 {
	num_spaces := txt.count(' ')
	if num_spaces < 1 {
		return 0
	}
	delta := block_w - w
	// println("num spc: $num_spaces")
	// println("delta: ${txt} w:$w bw:$block_w space_cw:$space_cw")
	res := f32(delta) / f32(num_spaces) / f32(space_cw)
	// println("res: $res")
	return res
}

// write out a text
pub fn (mut bmp BitMap) draw_text_block(text string, block Text_block) {
	mut x := block.x
	mut y := block.y
	mut y_base := int((bmp.tf.y_max - bmp.tf.y_min) * bmp.scale)

	// bmp.box(x, y, x + block.w, y + block.h, u32(0xFF00_0000))

	// spaces data
	mut space_cw, _ := bmp.tf.get_horizontal_metrics(u16(` `))
	space_cw = int(space_cw * bmp.scale)

	old_space_cw := bmp.space_cw

	mut offset_flag := f32(0) // default .left align
	if bmp.align == .right {
		offset_flag = 1
	} else if bmp.align == .center {
		offset_flag = 0.5
	}

	for txt in text.split_into_lines() {
		bmp.space_cw = old_space_cw
		mut w, mut h := bmp.get_bbox(txt)
		if w <= block.w || block.cut_lines == false {
			// println("Solid block!")
			left_offset := int((block.w - w) * offset_flag)
			if bmp.justify && (f32(w) / f32(block.w)) >= bmp.justify_fill_ratio {
				bmp.space_cw = old_space_cw + bmp.get_justify_space_cw(txt, w, block.w, space_cw)
			}
			bmp.set_pos(x + left_offset, y + y_base)
			bmp.draw_text(txt)
			//---- DEBUG ----
			// mut txt_w , mut txt_h := bmp.draw_text(txt)
			// bmp.box(x + left_offset,y+y_base - int((bmp.tf.y_min)*bmp.scale), x + txt_w + left_offset, y + y_base - int((bmp.tf.y_max) * bmp.scale), u32(0x00ff_0000) )
			//---------------
			y += y_base
		} else {
			// println("to cut: ${txt}")
			mut txt1 := txt.split(' ')
			mut c := txt1.len
			// mut done := false
			for c > 0 {
				tmp_str := txt1[0..c].join(' ')
				// println("tmp_str: ${tmp_str}")
				if tmp_str.len < 1 {
					break
				}

				bmp.space_cw = old_space_cw
				w, h = bmp.get_bbox(tmp_str)
				if w <= block.w {
					mut left_offset := int((block.w - w) * offset_flag)
					if bmp.justify && (f32(w) / f32(block.w)) >= bmp.justify_fill_ratio {
						// println("cut phase!")
						bmp.space_cw = 0.0
						w, h = bmp.get_bbox(tmp_str)
						left_offset = int((block.w - w) * offset_flag)
						bmp.space_cw = bmp.get_justify_space_cw(tmp_str, w, block.w, space_cw)
					} else {
						bmp.space_cw = old_space_cw
					}
					bmp.set_pos(x + left_offset, y + y_base)
					bmp.draw_text(tmp_str)
					//---- DEBUG ----
					// txt_w , txt_h := bmp.draw_text(tmp_str)
					// println("printing [${x},${y}] => '${tmp_str}' space_cw: $bmp.space_cw")
					// bmp.box(x + left_offset,y + y_base - int((bmp.tf.y_min)*bmp.scale), x + txt_w + left_offset, y + y_base - int((bmp.tf.y_max) * bmp.scale), u32(0x00ff_0000) )
					//---------------
					y += y_base
					txt1 = txt1[c..]
					c = txt1.len
					//---- DEBUG ----
					// txt2 := txt1.join(' ')
					// println("new string: ${txt2} len: ${c}")
					//---------------
				} else {
					c--
				}
			}
		}
	}

	bmp.space_cw = old_space_cw
}
