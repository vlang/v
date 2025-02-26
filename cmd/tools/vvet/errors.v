// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
import v.token
import term

pub enum ErrorKind {
	error
	warning
	notice
}

pub enum FixKind {
	unknown
	doc
	vfmt
	repeated_code
	long_fns
	empty_fn
	inline_fn
}

// ErrorType is used to filter out false positive errors under specific conditions
pub enum ErrorType {
	default
	space_indent
	trailing_space
}

@[minify]
pub struct VetError {
pub mut:
	kind ErrorKind @[required]
pub:
	// General message
	message   string @[required]
	details   string    // Details about how to resolve or fix the situation
	file_path string    // file where the error have origin
	pos       token.Pos // position in the file
	fix       FixKind   @[required]
	typ       ErrorType @[required]
}

fn (mut vt Vet) error(msg string, line int, fix FixKind) {
	pos := token.Pos{
		line_nr: line + 1
	}
	lock vt.errors {
		vt.errors << VetError{
			message:   msg
			file_path: vt.file
			pos:       pos
			kind:      .error
			fix:       fix
			typ:       .default
		}
	}
}

fn (mut vt Vet) warn(msg string, line int, fix FixKind) {
	pos := token.Pos{
		line_nr: line + 1
	}
	mut w := VetError{
		message:   msg
		file_path: vt.file
		pos:       pos
		kind:      .warning
		fix:       fix
		typ:       .default
	}
	if vt.opt.is_werror {
		w.kind = .error
		lock vt.errors {
			vt.errors << w
		}
	} else {
		lock vt.warns {
			vt.warns << w
		}
	}
}

fn (mut vt Vet) notice(msg string, line int, fix FixKind) {
	pos := token.Pos{
		line_nr: line + 1
	}
	lock vt.notices {
		vt.notices << VetError{
			message:   msg
			file_path: vt.file
			pos:       pos
			kind:      .notice
			fix:       fix
			typ:       .default
		}
	}
}

fn (mut vt Vet) notice_with_file(file string, msg string, line int, fix FixKind) {
	pos := token.Pos{
		line_nr: line + 1
	}
	lock vt.notices {
		vt.notices << VetError{
			message:   msg
			file_path: file
			pos:       pos
			kind:      .notice
			fix:       fix
			typ:       .default
		}
	}
}

fn (vt &Vet) e2string(err VetError) string {
	mut kind := '${err.kind}:'
	mut location := '${err.file_path}:${err.pos.line_nr}:'
	if vt.opt.use_color {
		kind = term.bold(match err.kind {
			.warning { term.magenta(kind) }
			.error { term.red(kind) }
			.notice { term.yellow(kind) }
		})
		location = term.bold(location)
	}
	return '${location} ${kind} ${err.message}'
}
