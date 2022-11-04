module fmt

import v.util

// By default, vfmt *always reformats all source code passed to it* in an uniform way.
// This works in the vast majority of the cases, providing a very nice default and
// consistent look & feel for most V codebases, and eliminating countless hours of bike
// shedding discussions about whether using spaces or tabs is better, and where {} and
// () should be put, and whether comments should be aligned or not.
//
// However, there are indeed situations, where careful manual formatting can increase
// understanding and maintainability of the code. For example, if you write matrix heavy
// code, you may want to preserve the look of your carefully written beautiful matrices,
// so that every reader of your code can bask in their glory.
//
// To enable such manual formatting, this file supports using `vfmt off` and `vfmt on`
// line comments, which can be used for turning off and on vfmt *locally* in selected
// blocks of code, while leaving the vast majority of the code, to be formatted by vfmt.
//
// TLDR:
// All lines after `// vfmt off` are passed on *without any modification* to the output.
// All lines after `// vfmt on` are going to be formatted, and then added to the output.

struct FormatState {
mut:
	is_vfmt_on           bool = true
	last_off_source_line int // the source line where // vfmt off was encountered first
	last_off_out_len     int // f.out.len when // vfmt off was encountered first
}

fn (mut fs FormatState) reset() {
	fs.is_vfmt_on = true
	fs.last_off_source_line = 0
	fs.last_off_out_len = 0
}

pub fn (mut f Fmt) vfmt_off(off_line int) {
	// only trigger once on on->off edges:
	if !f.format_state.is_vfmt_on {
		return
	}
	f.format_state.is_vfmt_on = false
	f.format_state.last_off_source_line = off_line
	f.format_state.last_off_out_len = f.out.len
}

pub fn (mut f Fmt) vfmt_on(on_line int) {
	// only trigger once on off->on edges:
	if f.format_state.is_vfmt_on {
		return
	}
	f.out.cut_to(f.format_state.last_off_out_len)
	f.out.writeln('')
	source_lines := f.get_source_lines()#[f.format_state.last_off_source_line + 1..on_line]
	for line in source_lines {
		f.out.writeln(line)
	}
	f.format_state.reset()
}

pub fn (mut f Fmt) get_source_lines() []string {
	if f.file.path != '' {
		return unsafe { util.cached_file2sourcelines(f.file.path) }
	}
	return f.source_text.split('\n')
}
