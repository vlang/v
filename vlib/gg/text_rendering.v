// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import gx

enum FontVariant {
	normal = 0
	bold
	mono
	italic
}

struct FTConfig {
	font_path             string
	custom_bold_font_path string
	scale                 f32 = 1.0
	font_size             int
	bytes_normal          []byte
	bytes_bold            []byte
	bytes_mono            []byte
	bytes_italic          []byte
}

struct StringToRender {
	x    int
	y    int
	text string
	cfg  gx.TextCfg
}

[if debug_font ?]
fn debug_font_println(s string) {
	println(s)
}
