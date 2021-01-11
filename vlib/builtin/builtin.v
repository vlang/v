// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

__global (
	g_m2_buf byteptr
	g_m2_ptr byteptr
)

// isnil returns true if an object is nil (only for C objects).
pub fn isnil(v voidptr) bool {
	return v == 0
}

/*
fn on_panic(f fn(int)int) {
	// TODO
}
*/

// print_backtrace shows a backtrace of the current call stack on stdout
pub fn print_backtrace() {
	// at the time of backtrace_symbols_fd call, the C stack would look something like this:
	// 1 frame for print_backtrace_skipping_top_frames
	// 1 frame for print_backtrace itself
	// ... print the rest of the backtrace frames ...
	// => top 2 frames should be skipped, since they will not be informative to the developer
	print_backtrace_skipping_top_frames(2)
}

__global (
	total_m    = i64(0)
	nr_mallocs = int(0)
)

fn __as_cast(obj voidptr, obj_type int, expected_type int) voidptr {
	if obj_type != expected_type {
		panic('as cast: cannot cast $obj_type to $expected_type')
	}
	return obj
}

// VAssertMetaInfo is used during assertions. An instance of it
// is filled in by compile time generated code, when an assertion fails.
pub struct VAssertMetaInfo {
pub:
	fpath   string // the source file path of the assertion
	line_nr int    // the line number of the assertion
	fn_name string // the function name in which the assertion is
	src     string // the actual source line of the assertion
	op      string // the operation of the assertion, i.e. '==', '<', 'call', etc ...
	llabel  string // the left side of the infix expressions as source
	rlabel  string // the right side of the infix expressions as source
	lvalue  string // the stringified *actual value* of the left side of a failed assertion
	rvalue  string // the stringified *actual value* of the right side of a failed assertion
}
fn __print_assert_failure(i &VAssertMetaInfo) {
	eprintln('${i.fpath}:${i.line_nr+1}: FAIL: fn ${i.fn_name}: assert ${i.src}')
	if i.op.len > 0 && i.op != 'call' {
		eprintln('   left value: ${i.llabel} = ${i.lvalue}')
		if i.rlabel == i.rvalue {
			eprintln('  right value: $i.rlabel')
		}
		else {
			eprintln('  right value: ${i.rlabel} = ${i.rvalue}')
		}
	}
}

pub struct MethodArgs {
pub:
	typ int
}

pub struct FunctionData {
pub:
	name        string
	attrs       []string
	args        []MethodArgs
	return_type int
	typ         int
}

pub struct FieldData {
pub:
	name   string
	attrs  []string
	is_pub bool
	is_mut bool
	typ    int
}
