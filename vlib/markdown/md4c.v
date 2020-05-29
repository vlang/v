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

#flag -I @VROOT/thirdparty/md4c
#flag @VROOT/thirdparty/md4c/md4c.o
#include "md4c.h"

type BlockFn = fn (t MD_BLOCKTYPE, d voidptr, u voidptr) int
type SpanFn = fn (t MD_SPANTYPE, d voidptr, u voidptr) int
type TextFn = fn (t MD_TEXTTYPE, tx charptr, s u32, u voidptr) int
type DebugFn = fn (m charptr, u voidptr)

pub enum MD_BLOCKTYPE {
	md_block_doc = 0
	md_block_quote
	md_block_ul
	md_block_ol
	md_block_li
	md_block_hr
	md_block_h
	md_block_code
	md_block_html
	md_block_p
	md_block_table
	md_block_thead
	md_block_tbody
	md_block_tr
	md_block_th
	md_block_td
}

pub enum MD_TEXTTYPE {
	md_text_normal = 0
	md_text_null_char
	md_text_br
	md_text_softbr
	md_text_entity
	md_text_code
	md_text_html
	md_text_latexmath
}

pub enum MD_SPANTYPE {
	md_span_em
	md_span_strong
	md_span_a
	md_span_img
	md_span_code
	md_span_del
	md_span_latexmath
	md_span_latexmath_display
	md_span_wikilink
	md_span_u
}

pub enum MD_ALIGN {
	md_align_default = 0
	md_align_left
	md_align_center
	md_align_right
}

pub struct C.MD_PARSER {
pub:
	abi_version u32
	flags u32
	enter_block BlockFn
	leave_block BlockFn
	enter_span SpanFn
	leave_span SpanFn
	text TextFn
	debug_log DebugFn
}

pub struct C.MD_ATTRIBUTE {
pub:
	text charptr
	size u32
	substr_types MD_TEXTTYPE
	substr_offsets u32
}

pub struct C.MD_BLOCK_UL_DETAIL {
pub:
	is_tight int
	mark byte
}

pub struct C.MD_BLOCK_OL_DETAIL {
pub:
	start u32
	is_tight int
	mark_delimiter byte
}

pub struct C.MD_BLOCK_LI_DETAIL {
pub:
	is_task int
	task_mark byte
	task_mark_offset u32
}

pub struct C.MD_BLOCK_H_DETAIL {
pub:
	level u32
}

pub struct C.MD_BLOCK_CODE_DETAIL {
pub:
	info C.MD_ATTRIBUTE
	lang C.MD_ATTRIBUTE
	fence_char byte
}

pub struct C.MD_BLOCK_TD_DETAIL {
pub:
	align MD_ALIGN
}

pub struct C.MD_SPAN_A_DETAIL {
pub:
	href C.MD_ATTRIBUTE
	title C.MD_ATTRIBUTE
}

pub struct C.MD_SPAN_IMG_DETAIL {
pub:
	src C.MD_ATTRIBUTE
	title C.MD_ATTRIBUTE
}

pub struct C.MD_SPAN_WIKILINK_DETAIL {
pub:
	target C.MD_ATTRIBUTE
}

pub fn C.md_parse(text charptr, size u32, parser &C.MD_PARSER, userdata voidptr) int

pub fn new(parser_flags u32, enter_block_cb BlockFn, leave_block_cb BlockFn, enter_span_cb SpanFn, leave_span_cb SpanFn, text_cb TextFn, debug_cb DebugFn) C.MD_PARSER {
	return C.MD_PARSER{
		abi_version: 0
		flags: parser_flags
		enter_block: enter_block_cb
		leave_block: leave_block_cb
		enter_span: enter_span_cb
		leave_span: leave_span_cb
		text: text_cb
		debug_log: debug_cb
	}
}

pub fn parse(text charptr, size u32, parser &C.MD_PARSER, userdata voidptr) int {
	return C.md_parse(text, size, parser, userdata)
}