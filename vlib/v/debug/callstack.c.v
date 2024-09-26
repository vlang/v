// Copyright (c) 2019-2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
@[has_globals]
module debug

// function call location trace
@[markused]
pub struct FnTrace {
pub:
	name string
	file string
	line i64
}

@[markused]
__global g_callstack = []FnTrace{}

// dump_callstack dumps callstack to the user
@[markused]
pub fn dump_callstack() {
	bar := '-'.repeat(50).str
	C.printf(c'Backtrace:\n')
	C.printf(c'%s\n', bar)
	callstack_len := g_callstack.len
	for i := 0; i < callstack_len; i++ {
		item := g_callstack[i]
		C.printf(c'%s:%-4d | %s> %s\n', &char(item.file.str), item.line, ' '.repeat(i).str,
			item.name)
	}
	C.printf(c'%s\n', bar)
}

// callstack retrieves the supplied stack frame based on supplied depth
@[markused]
pub fn callstack(depth int) ?FnTrace {
	if depth >= g_callstack.len {
		return none
	}
	return g_callstack[depth]
}
