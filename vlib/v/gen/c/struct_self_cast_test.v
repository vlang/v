module main

import os
import v.cmdexec

fn testsuite_begin() {
	// The MSVC CI jobs put `-cc msvc` in VFLAGS; each generation here picks its compiler.
	os.unsetenv('VFLAGS')
}

// A cast whose operand already has the target's C struct type emits the operand: C casts
// only to scalar types, and MSVC rejects `(Point)(value)` (C2440) that GCC and Clang accept.
// The MSVC lowering drops such casts only for the struct typedefs that V itself writes,
// so a C struct from a header kept its cast.
fn test_struct_cast_to_its_own_c_type_emits_the_value() {
	root := os.join_path(os.vtmp_dir(), 'cgen_struct_self_cast_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'struct_self_cast' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'point.h'), 'typedef struct Point {\n\tint x;\n} Point;\n') or {
		panic(err)
	}
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#include "@VMODROOT/point.h"

@[typedef]
struct C.Point {
	x int
}

type PointAlias = C.Point

struct Plain {
	x int
}

type PlainAlias = Plain

fn take_point(p PointAlias) int {
	return p.x
}

fn main() {
	p := PointAlias(C.Point{
		x: 7
	})
	src := &p
	copied := PointAlias(*src)
	plain := Plain{
		x: 3
	}
	plain_ref := &plain
	plain_copy := PlainAlias(*plain_ref)
	println(take_point(PointAlias(*src)) + copied.x + plain_copy.x)
}
') or {
		panic(err)
	}
	for flavor in [['-os', 'windows', '-cc', 'msvc'], []string{}] {
		c_file := os.join_path(root, 'main${flavor.len}.c')
		generated := cmdexec.run(@VEXE, ['-gc', 'none', ...flavor, '-o', c_file, source])
		assert generated.exit_code == 0, generated.output
		c := os.read_file(c_file) or { panic(err) }
		assert c.contains('Point p = ((Point){.x = 7});'), flavor.str()
		assert c.contains('Point copied = (*src);'), flavor.str()
		assert c.contains('take_point((*src))'), flavor.str()
		assert c.contains('main__Plain plain_copy = (*plain_ref);'), flavor.str()
		assert !c.contains('(Point)('), flavor.str()
		assert !c.contains('(main__Plain)('), flavor.str()
	}
}
