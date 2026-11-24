module main

import os
import v.cmdexec

fn testsuite_begin() {
	// The MSVC CI jobs put `-cc msvc` in VFLAGS; each generation here picks its compiler.
	os.unsetenv('VFLAGS')
}

const wide_enum_source = '#include "@VMODROOT/probe.h"

enum Wide {
	small = 1
	big   = 1099511627776
	after
}

enum Symbolic {
	narrow = int(C.probe_one) + 2
	wide   = int(C.probe_one) + 2 ** 40
}

enum Narrow {
	a
	b = 5
	c
}

fn main() {
	println(int(Wide.big) + int(Wide.after) + int(Symbolic.wide) + int(Narrow.c))
}
'

// MSVC gives every enumerator the type `int` and truncates a wider value, so a plain
// enum that can hold a value outside `int` is written as `#define`d 64 bit constants.
// GCC, Clang and TinyCC widen the enumeration, so their C keeps the enumeration.
fn test_plain_enum_values_wider_than_c_int_keep_64_bits_for_msvc() {
	root := os.join_path(os.vtmp_dir(), 'cgen_msvc_wide_enum_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'msvc_wide_enum' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'probe.h'), 'enum {\n\tprobe_one = 1\n};\n') or {
		panic(err)
	}
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, wide_enum_source) or { panic(err) }
	msvc_file := os.join_path(root, 'msvc.c')
	msvc := cmdexec.run(@VEXE, ['-gc', 'none', '-os', 'windows', '-cc', 'msvc', '-o', msvc_file,
		source])
	assert msvc.exit_code == 0, msvc.output
	msvc_c := os.read_file(msvc_file) or { panic(err) }
	wide_decl := 'typedef i64 main__Wide;
#define Wide____v3_internal_symbol_macro_small ((main__Wide)(1))
#define Wide__big ((main__Wide)(1099511627776))
#define Wide__after ((main__Wide)(1099511627777))
'
	symbolic_decl := 'typedef i64 main__Symbolic;
#define Symbolic__narrow ((main__Symbolic)(((i64)(((i64)(probe_one)) + (2)))))
#define Symbolic__wide ((main__Symbolic)(((i64)(((i64)(probe_one)) + (1099511627776)))))
'
	narrow_decl := 'typedef enum {
	Narrow__a = 0,
	Narrow__b = 5,
	Narrow__c = 6,
} main__Narrow;
'
	assert msvc_c.contains(wide_decl)
	assert msvc_c.contains(symbolic_decl)
	assert msvc_c.contains(narrow_decl)
	native_file := os.join_path(root, 'native.c')
	native := cmdexec.run(@VEXE, ['-gc', 'none', '-o', native_file, source])
	assert native.exit_code == 0, native.output
	native_c := os.read_file(native_file) or { panic(err) }
	assert native_c.contains('\tWide__big = 1099511627776,\n')
	assert native_c.contains('\tSymbolic__wide = ((i64)(((i64)(probe_one)) + (1099511627776))),\n')
	assert native_c.contains(narrow_decl)
	// Clang's MSVC mode applies MSVC's enumerator rules; check the values it computes.
	clang := os.find_abs_path_of_executable('clang') or { return }
	probe := os.join_path(root, 'probe.c')
	os.write_file(probe, 'typedef long long i64;
#include "probe.h"
${wide_decl}${symbolic_decl}${narrow_decl}_Static_assert(Wide__big == 1099511627776LL, "Wide.big");
_Static_assert(Wide__after == 1099511627777LL, "Wide.after");
_Static_assert(Symbolic__wide == 1099511627777LL, "Symbolic.wide");
_Static_assert(Narrow__c == 6, "Narrow.c");
') or {
		panic(err)
	}
	checked := os.exec([clang, '-target', 'x86_64-pc-windows-msvc', '-fsyntax-only', probe])
	assert checked.exit_code == 0, checked.output
}
