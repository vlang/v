// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
[has_globals]
module builtin

// isnil returns true if an object is nil (only for C objects).
[inline]
pub fn isnil(v voidptr) bool {
	return v == 0
}

/*
fn on_panic(f fn(int)int) {
	// TODO
}
*/

struct VCastTypeIndexName {
	tindex int
	tname  string
}

// will be filled in cgen
__global as_cast_type_indexes []VCastTypeIndexName

fn __as_cast(obj voidptr, obj_type int, expected_type int) voidptr {
	if obj_type != expected_type {
		mut obj_name := as_cast_type_indexes[0].tname.clone()
		mut expected_name := as_cast_type_indexes[0].tname.clone()
		for x in as_cast_type_indexes {
			if x.tindex == obj_type {
				obj_name = x.tname.clone()
			}
			if x.tindex == expected_type {
				expected_name = x.tname.clone()
			}
		}
		panic('as cast: cannot cast `${obj_name}` to `${expected_name}`')
	}
	return obj
}

// VAssertMetaInfo is used during assertions. An instance of it is filled in by
// compile time generated code, when an assertion fails.
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
	message string // the value of the `message` from `assert cond, message`
	has_msg bool   // false for assertions like `assert cond`, true for `assert cond, 'oh no'`
}

// free frees the memory occupied by the assertion meta data. It is called automatically by
// the code, that V's test framework generates, after all other callbacks have been called.
[manualfree; unsafe]
pub fn (ami &VAssertMetaInfo) free() {
	unsafe {
		ami.fpath.free()
		ami.fn_name.free()
		ami.src.free()
		ami.op.free()
		ami.llabel.free()
		ami.rlabel.free()
		ami.lvalue.free()
		ami.rvalue.free()
		ami.message.free()
	}
}

fn __print_assert_failure(i &VAssertMetaInfo) {
	eprintln('${i.fpath}:${i.line_nr + 1}: FAIL: fn ${i.fn_name}: assert ${i.src}')
	if i.op.len > 0 && i.op != 'call' {
		eprintln('   left value: ${i.llabel} = ${i.lvalue}')
		if i.rlabel == i.rvalue {
			eprintln('  right value: ${i.rlabel}')
		} else {
			eprintln('  right value: ${i.rlabel} = ${i.rvalue}')
		}
		if i.has_msg {
			eprintln('      message: ${i.message}')
		}
	}
}

// MethodArgs holds type information for function and/or method arguments.
pub struct MethodArgs {
pub:
	typ  int
	name string
}

// FunctionData holds information about a parsed function.
pub struct FunctionData {
pub:
	name        string
	attrs       []string
	args        []MethodArgs
	return_type int
	typ         int
}

// FieldData holds information about a field. Fields reside on structs.
pub struct FieldData {
pub:
	name      string
	attrs     []string
	is_pub    bool
	is_mut    bool
	is_shared bool
	typ       int
}

pub enum AttributeKind {
	plain // [name]
	string // ['name']
	number // [123]
	comptime_define // [if name]
}

pub struct StructAttribute {
pub:
	name    string
	has_arg bool
	arg     string
	kind    AttributeKind
}

[markused]
fn v_segmentation_fault_handler(signal int) {
	eprintln('signal 11: segmentation fault')
	$if use_libbacktrace ? {
		eprint_libbacktrace(1)
	} $else {
		print_backtrace()
	}
	exit(128 + 11)
}
