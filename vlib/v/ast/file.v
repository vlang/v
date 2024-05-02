// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.errors

// Each V source file is represented by one File structure.
// When the V compiler runs, the parser will fill an []File.
// That array is then passed to V's checker.
@[heap]
pub struct File {
pub:
	nr_lines      int    // number of source code lines in the file (including newlines and comments)
	nr_bytes      int    // number of processed source code bytes
	mod           Module // the module of the source file (from `module xyz` at the top)
	global_scope  &Scope = unsafe { nil }
	is_test       bool // true for _test.v files
	is_generated  bool // true for `[generated] module xyz` files; turn off notices
	is_translated bool // true for `[translated] module xyz` files; turn off some checks
pub mut:
	idx              int    // index in an external container; can be used to refer to the file in a more efficient way, just by its integer index
	path             string // absolute path of the source file - '/projects/v/file.v'
	path_base        string // file name - 'file.v' (useful for tracing)
	scope            &Scope = unsafe { nil }
	stmts            []Stmt            // all the statements in the source file
	imports          []Import          // all the imports
	auto_imports     []string          // imports that were implicitly added
	embedded_files   []EmbeddedFile    // list of files to embed in the binary
	imported_symbols map[string]string // used for `import {symbol}`, it maps symbol => module.symbol
	errors           []errors.Error    // all the checker errors in the file
	warnings         []errors.Warning  // all the checker warnings in the file
	notices          []errors.Notice   // all the checker notices in the file
	generic_fns      []&FnDecl
	global_labels    []string // from `asm { .globl labelname }`
	template_paths   []string // all the .html/.md files that were processed with $tmpl
	unique_prefix    string   // a hash of the `.path` field, used for making anon fn generation unique
}

@[unsafe]
pub fn (f &File) free() {
	unsafe {
		f.path.free()
		f.path_base.free()
		f.scope.free()
		f.stmts.free()
		f.imports.free()
		f.auto_imports.free()
		f.embedded_files.free()
		f.imported_symbols.free()
		f.errors.free()
		f.warnings.free()
		f.notices.free()
		f.global_labels.free()
	}
}
