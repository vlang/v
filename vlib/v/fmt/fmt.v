// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

struct Fmt {
	// vfmt fields TODO move to a separate struct
	// fmt_out        strings.Builder
	fmt_lines                []string
	// fmt_line   string
	fmt_indent               int
	fmt_line_empty           bool
	// fmt_needs_nl bool

}
