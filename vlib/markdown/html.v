/*
 * MD4C: Markdown parser for C
 * (http://github.com/mity/md4c)
 *
 * Copyright (c) 2016-2019 Martin Mitas
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

module markdown

// import net.urllib
import strings

fn C.strchr(str charptr, ch int) byte

// workaround
struct C.MD_BLOCK_H_DETAIL {
pub:
	level u32
}

struct C.MD_SPAN_WIKILINK_DETAIL {
pub:
	target C.MD_ATTRIBUTE
}

struct MdHtml {
	process_output ProcessFn
	userdata voidptr
	flags u32
mut:
	image_nesting_level int
	escape_map []byte = []byte{len: 256}
}

const (
	need_html_esc_flag = 0x1
	need_url_esc_flag  = 0x2
	md_html_flag_debug = 0x0001
	md_html_flag_verbatim_entities = 0x0002
	md_html_flag_skip_utf8_bom = 0x0004
)

type AppendFn = fn (r &MdHtml, t charptr, s u32)
type ProcessFn = fn (t charptr, s u32, d voidptr)

[inline]
fn render_verbatim(r &MdHtml, text charptr, size u32) {
	r.process_output(text, size, r.userdata)
}

fn r_verbatim(r &MdHtml, verbatim string) {
	render_verbatim(r, charptr(verbatim.str), verbatim.len)
}

fn render_html_escaped(r &MdHtml, data charptr, size u32) {
	mut beg := 0
	mut off := 0
	need_html_esc := fn (r &MdHtml, ch byte) byte {
		return r.escape_map[u32(ch)] & need_html_esc_flag
	}

	for {
		for off + 3 < size && bool(need_html_esc(r, data[off+0])) && !bool(need_html_esc(r, data[off+1])) && !bool(need_html_esc(r, data[off+2])) && !bool(need_html_esc(r, data[off+3])) {
			off += 4
		}

		for off < size && !bool(need_html_esc(r, data[off])) {
			off++
		}

		if off > beg {
			render_verbatim(r, data + beg, off - beg)
		}

		if off < size {
			match data[off] {
				`&` { r_verbatim(r, '&amp;') }
				`<` { r_verbatim(r, '&lt;') }
				`>` { r_verbatim(r, '&gt;') }
				`"` { r_verbatim(r, '&quot;') }
				else {}
			}
			off++
		} else {
			break
		}
		beg = off
	}
}

fn render_url_escaped(r &MdHtml, data charptr, size u32) {
	// escaped := urllib.query_escape(tos3(data))
	render_verbatim(r, data, 3)
}

fn hex_val(ch byte) u32 {
	if ch.is_digit() {
		return ch - `0`
	} else if `A` <= ch && ch <= `Z` {
		return ch - `A` + 10
	} else {
		return ch - `a` + 10
	}
}

fn render_utf8_codepoint(r &MdHtml, codepoint u32, fn_append AppendFn) {
	utf8_replacement_char := [0xef, 0xbf, 0xbd]
	mut utf8 := [4]u32
	mut n := 0

	if codepoint <= 0x7f {
		n = 1
		utf8[0] = codepoint
	} else if codepoint <= 0x7ff {
		n = 2
		utf8[0] = 0xc0 | ((codepoint >> 6) & 0x1f)
		utf8[1] = 0x80 | ((codepoint >> 0) & 0x3f)
	} else if codepoint <= 0xffff {
		n = 3
		utf8[0] = 0xe0 | ((codepoint) >> 12 & 0xf)
		utf8[1] = 0x80 + ((codepoint) >> 6  & 0x3f)
		utf8[2] = 0x80 + ((codepoint) >> 0  & 0x3f)
	} else {
		n = 4
		utf8[0] = 0xf0 | ((codepoint >> 18) & 0x7)
		utf8[1] = 0x80 + ((codepoint >> 12) & 0x3f)
		utf8[2] = 0x80 + ((codepoint >> 6)  & 0x3f)
		utf8[3] = 0x80 + ((codepoint >> 0)  & 0x3f)
	}

	if 0 < codepoint && codepoint <= 0x10ffff {
		fn_append(r, charptr(&utf8), n)
	} else {
		fn_append(r, charptr(&utf8_replacement_char), 3)
	}
}

fn render_entity(r &MdHtml, text charptr, size u32, fn_append AppendFn) {
	if bool(r.flags & u32(md_html_flag_verbatim_entities)) {
		fn_append(r, text, size)
		return
	}

	if size > 3 && text[1] == `#` {
		mut codepoint := u32(0)

		if byte(text[2]) in [`x`, `X`] {
			for i := 3; u32(i) < size - 1; i++ {
				codepoint = 16 * codepoint + hex_val(text[i])
			}
		} else {
			for i := 2; u32(i) < size - 1; i++ {
				codepoint = 10 * codepoint + byte(text[i] - `0`)
			}
		}

		render_utf8_codepoint(r, codepoint, fn_append)
		return
	} else {
		mut ent := entity_lookup(text, size)
		render_utf8_codepoint(r, ent.codepoints[0], fn_append)

		if bool(ent.codepoints[1]) {
			render_utf8_codepoint(r, ent.codepoints[1], fn_append)
		}

		return
	}

	fn_append(r, text, size)
}

[inline]
fn int_to_md_text_type(x int) MD_TEXTTYPE {
	// NB: this function is needed just to make g++ happy
	return x
}
fn render_attribute(r &MdHtml, attr &C.MD_ATTRIBUTE, fn_append AppendFn) {
	for i := 0; attr.substr_offsets[i] < attr.size; i++ {
		typ := int_to_md_text_type( attr.substr_types[i] )
		off := attr.substr_offsets[i]
		size := attr.substr_offsets[i+1] - off
		text := attr.text + off

		match typ {
			.md_text_null_char {
				render_utf8_codepoint(r, 0x0000, render_verbatim)
			}
			.md_text_entity {
				render_entity(r, text, size, fn_append)
			}
			else {
				fn_append(r, charptr(tos3(text)[..size].str), size)
			}
		}
	}
}

fn render_open_ol_block(r &MdHtml, det &C.MD_BLOCK_OL_DETAIL) {
	if det.start == 1 {
		r_verbatim(r, '<ol>\n')
		return
	}

	r_verbatim(r, '<ol start="${det.start}">\n')
}

fn render_open_li_block(r &MdHtml, det &C.MD_BLOCK_LI_DETAIL) {
	if bool(det.is_task) {
		r_verbatim(r, '<li class="task-lisk-item"> <input type="checkbox" class="task-list-item-checkbox" disabled')

		if det.task_mark in [`x`, `X`] {
			r_verbatim(r, ' checked')
		}
		r_verbatim(r, '>')
	} else {
		r_verbatim(r, '<li>')
	}
}

fn render_open_code_block(r &MdHtml, det &C.MD_BLOCK_CODE_DETAIL) {
	r_verbatim(r, '<pre><code')

	if det.lang.text != 0 {
		r_verbatim(r, ' class="language-')
		render_attribute(r, &det.lang, render_html_escaped)
		r_verbatim(r, '"')
	}

	r_verbatim(r, '>')
}

fn render_open_td_block(r &MdHtml, cell_type charptr, det &C.MD_BLOCK_TD_DETAIL) {
	r_verbatim(r, '<')
	r_verbatim(r, tos3(cell_type))

	match det.align {
		C.MD_ALIGN_LEFT { r_verbatim(r, ' align="left">') }
		C.MD_ALIGN_CENTER { r_verbatim(r, ' align="center">') }
		C.MD_ALIGN_RIGHT { r_verbatim(r, ' align="right">') }
		else { r_verbatim(r, '>') }
	}
}

fn render_open_a_span(r &MdHtml, det &C.MD_SPAN_A_DETAIL) {
	r_verbatim(r, '<a href="')
	render_attribute(r, &det.href, render_url_escaped)

	if det.title.text != 0 {
		r_verbatim(r, '" title="')
		render_attribute(r, &det.title, render_html_escaped)
	}

	r_verbatim(r, '">')
}

fn render_open_img_span(r &MdHtml, det &C.MD_SPAN_IMG_DETAIL) {
	r_verbatim(r, '<img src="')
	render_attribute(r, &det.src, render_url_escaped)
	r_verbatim(r, '" alt="')
	r.image_nesting_level++
}

fn render_close_img_span(r &MdHtml, det &C.MD_SPAN_IMG_DETAIL) {

	if det.title.text != 0 {
		r_verbatim(r, '" title="')
		render_attribute(r, &det.title, render_html_escaped)
	}

	r_verbatim(r, '">')
	r.image_nesting_level--
}

// fn render_open_wikilink_span(r &MdHtml, det &C.MD_SPAN_WIKILINK_DETAIL) {
// 	r_verbatim(r, '<x-wikilink data-target="')
// 	render_attribute(r, det.target, render_html_escaped)
// 	r_verbatim(r, '">')
// }

fn enter_block_callback(typ MD_BLOCKTYPE, detail voidptr, userdata voidptr) int {
	head := ['<h1>', '<h2>', '<h3>', '<h4>', '<h5>', '<h6>']
	r := &MdHtml(userdata)

	match typ {
		.md_block_doc {}
		.md_block_quote { r_verbatim(r, '<blockquote>\n') }
		.md_block_ul { r_verbatim(r, '<ul>\n') }
		.md_block_ol { render_open_ol_block(r, &C.MD_BLOCK_OL_DETAIL(detail)) }
		.md_block_li { render_open_li_block(r, &C.MD_BLOCK_LI_DETAIL(detail)) }
		.md_block_hr { r_verbatim(r, '<hr />\n') }
		.md_block_h { r_verbatim(r, head[&C.MD_BLOCK_H_DETAIL(detail).level - 1]) }
		.md_block_code { render_open_code_block(r, &C.MD_BLOCK_CODE_DETAIL(detail)) }
		.md_block_html {}
		.md_block_p { r_verbatim(r, '<p>') }
		.md_block_table { r_verbatim(r, '<table>\n') }
		.md_block_thead { r_verbatim(r, '<thead>\n') }
		.md_block_tbody { r_verbatim(r, '<tbody>\n') }
		.md_block_tr { r_verbatim(r, '<tr>\n') }
		.md_block_th { render_open_td_block(r, charptr('th'.str), &C.MD_BLOCK_TD_DETAIL(detail)) }
		.md_block_td { render_open_td_block(r, charptr('td'.str), &C.MD_BLOCK_TD_DETAIL(detail)) }
	}

	return 0
}

fn leave_block_callback(typ MD_BLOCKTYPE, detail voidptr, userdata voidptr) int {
	head := ['</h1>', '</h2>', '</h3>', '</h4>', '</h5>', '</h6>']
	r := &MdHtml(userdata)

	match typ {
		.md_block_doc {}
		.md_block_quote { r_verbatim(r, '</blockquote>\n') }
		.md_block_ul { r_verbatim(r, '</ul>\n') }
		.md_block_ol { r_verbatim(r, '</ol>\n') }
		.md_block_li { r_verbatim(r, '</li>\n') }
		.md_block_hr {}
		.md_block_h { r_verbatim(r, head[&C.MD_BLOCK_H_DETAIL(detail).level - 1]) }
		.md_block_code { r_verbatim(r, '</code></pre>\n') }
		.md_block_html {}
		.md_block_p { r_verbatim(r, '</p>\n') }
		.md_block_table { r_verbatim(r, '</table>\n') }
		.md_block_thead { r_verbatim(r, '</thead>\n') }
		.md_block_tbody { r_verbatim(r, '</tbody>\n') }
		.md_block_tr { r_verbatim(r, '</tr>\n') }
		.md_block_th { r_verbatim(r, '</th>\n') }
		.md_block_td { r_verbatim(r, '</td>\n') }
	}

	return 0
}

fn enter_span_callback(typ MD_SPANTYPE, detail voidptr, userdata voidptr) int {
	r := &MdHtml(userdata)

	if r.image_nesting_level > 0 {
		return 0
	}

	match typ {
		.md_span_em { r_verbatim(r, '<em>') }
		.md_span_strong { r_verbatim(r, '<strong>') }
		.md_span_u { r_verbatim(r, '<u>') }
		.md_span_a { render_open_a_span(r, &C.MD_SPAN_A_DETAIL(detail)) }
		.md_span_img { render_open_img_span(r, &C.MD_SPAN_IMG_DETAIL(detail)) }
		.md_span_code { r_verbatim(r, '<code>') }
		.md_span_del { r_verbatim(r, '<del>') }
		.md_span_latexmath { r_verbatim(r, '<x-equation>') }
		.md_span_latexmath_display { r_verbatim(r, '<x-equation type="') }
		// .md_span_wikilink { render_open_wikilink_span(r, &C.MD_SPAN_WIKILINK_DETAIL(detail)) }
		.md_span_wikilink {}
	}
}

fn leave_span_callback(typ MD_SPANTYPE, detail voidptr, userdata voidptr) int {
	r := &MdHtml(userdata)

	if r.image_nesting_level > 0 {
		if r.image_nesting_level == 1 && typ == .md_span_img {
			render_close_img_span(r, &C.MD_SPAN_IMG_DETAIL(detail))
		}
		return 0
	}

	match typ {
		.md_span_em { r_verbatim(r, '</em>') }
		.md_span_strong { r_verbatim(r, '</strong>') }
		.md_span_u { r_verbatim(r, '</u>') }
		.md_span_a { r_verbatim(r, '</a>') }
		.md_span_img { render_close_img_span(r, &C.MD_SPAN_IMG_DETAIL(detail)) }
		.md_span_code { r_verbatim(r, '</code>') }
		.md_span_del { r_verbatim(r, '</del>') }
		.md_span_latexmath { r_verbatim(r, '</x-equation>') }
		.md_span_latexmath_display { r_verbatim(r, '</x-equation>') }
		// .md_span_wikilink { r_verbatim(r, '</x-wikilink>')  }
		.md_span_wikilink {}
	}
}

fn text_callback(typ MD_TEXTTYPE, text charptr, size u32, userdata voidptr) int {
	r := &MdHtml(userdata)

	match typ {
		.md_text_null_char { render_utf8_codepoint(r, 0x0000, render_verbatim) }
		.md_text_br { r_verbatim(r, if r.image_nesting_level == 0 { '<br />' } else { '' }) }
		.md_text_softbr { r_verbatim(r, if r.image_nesting_level == 0 { '\n' } else { '' }) }
		.md_text_html { render_verbatim(r, charptr(tos3(text)[..size].str), size) }
		.md_text_entity { render_entity(r, text, size, render_html_escaped) }
		else { render_html_escaped(r, charptr(tos3(text)[..size].str), size) }
	}

	return 0
}

fn debug_log_callback(msg charptr, userdata voidptr) {
	r := &MdHtml(userdata)
	if bool(r.flags & u32(md_html_flag_debug)) {
		eprintln('MD4C: ${tos3(msg)}')
	}
}

fn md_html(orig_input charptr, orig_input_size u32, process_output ProcessFn, userdata voidptr, parser_flags u32, renderer_flags u32) int {
	parser := new(parser_flags, enter_block_callback, leave_block_callback, enter_span_callback, leave_span_callback, text_callback, debug_log_callback)
	mut render := MdHtml{
		process_output: process_output
		userdata: userdata
		flags: renderer_flags
		image_nesting_level: 0
	}

	chrset1 := [`"`, `&`, `<` `>`]
	chrset2 := [`-`, `_`, `.`, `+`, `!`, `*`, `(`, `)`, `,`, `%`, `#`, `@`, `?` `=`, `;`, `:`, `/`, `,` `+`, `$`]

	mut input := tos3(orig_input)
	mut input_size := orig_input_size

	for i := 0; i < 256; i++ {
		ch := byte(i)

		if ch in chrset1 {
			render.escape_map[i] |= need_html_esc_flag
		}

		if (ch.is_letter() || ch.is_digit()) && ch !in chrset2 {
			render.escape_map[i] |= need_url_esc_flag
		} 
	}

	// TODO
	// if bool(renderer_flags & u32(md_html_flag_skip_utf8_bom)) && sizeof(C.MD_CHAR) == 1 {
		// bom := [0xef, 0xbb, 0xbf]

		// if input_size >= sizeof(bom) && C.memcmp(orig_input, charptr(bom.str().str), sizeof(bom)) == 0 {
			//TODO
			// input += sizeof(bom)
			// input_size -= sizeof(bom)
		// }
	// } 

	return parse(charptr(input.str), input_size, &parser, &render)
}

fn write_data(sb &strings.Builder, txt string) {
	sb.write(txt)
}

pub fn to_html(input string) string {
	mut wr := strings.new_builder(200)

	output_fn := fn (txt charptr, s u32, d voidptr) {
		write_data(&strings.Builder(d), tos3(txt))
	}

	md_html(charptr(input.str), input.len, output_fn, &wr, C.MD_DIALECT_GITHUB, 0)
	return wr.str()
}
