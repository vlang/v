module main

import os
import v.cmdexec

// Translated C offsets a `voidptr` by bytes with `+=` and `-=`. C has no
// arithmetic on `void*`: GCC, Clang and TinyCC accept it as an extension, MSVC
// rejects it (C2036), so cgen must offset through `u8*`.
const translated_voidptr_offset_source = '@[translated]
module main

struct Holder {
mut:
	p voidptr
}

fn offset_local(p voidptr, n int) {
	mut raw := p
	raw += n
	raw -= true
	println(raw)
}

fn offset_field(mut h Holder) {
	h.p += 4
	h.p -= u8(2)
}

fn offset_elements(p voidptr) {
	mut values := [p]
	values[0] += 2
	mut fixed := [p, p]!
	fixed[1] -= 1
	mut slots := {
		"a": p
	}
	slots["a"] += 3
	mut slot := unsafe { &fixed[0] }
	unsafe {
		slot[0] += 1
	}
	println(values[0] == fixed[1])
	println(slots.len)
}

fn main() {
	x := 0
	offset_local(unsafe { voidptr(&x) }, 1)
	mut h := Holder{}
	offset_field(mut h)
	offset_elements(unsafe { voidptr(&x) })
}
'

fn test_translated_voidptr_offsets_use_byte_pointers() ! {
	root := os.join_path(os.vtmp_dir(), 'translated_voidptr_offset_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'input.v')
	generated := os.join_path(root, 'output.c')
	os.write_file(source, translated_voidptr_offset_source)!
	compiled := cmdexec.run(@VEXE, ['-gc', 'none', '-nocache', '-o', generated, source])
	assert compiled.exit_code == 0, compiled.output
	c := os.read_file(generated)!
	local := translated_voidptr_fn_body(c, 'offset_local')
	assert local.count('(u8*)(') == 2, local
	field := translated_voidptr_fn_body(c, 'offset_field')
	assert field.count('(u8*)(') == 2, field
	elements := translated_voidptr_fn_body(c, 'offset_elements')
	// The dynamic array, fixed array, map value and pointer index updates.
	assert elements.count('(u8*)(') == 4, elements
	cc := os.find_abs_path_of_executable('cc') or { return }
	mut cc_args := ['-std=gnu11', '-fsyntax-only']
	if os.exec([cc, '--version']).output.contains('clang') {
		cc_args << ['-Wno-everything', '-Wpointer-arith', '-Werror=pointer-arith']
	} else {
		cc_args << ['-Wpointer-arith', '-Werror=pointer-arith']
	}
	checked := os.exec([cc, ...cc_args, generated])
	assert checked.exit_code == 0, checked.output
}

fn translated_voidptr_fn_body(c string, name string) string {
	assert c.contains('void ${name}('), 'missing generated function ${name}'
	return c.all_after_last('void ${name}(').all_after('{').all_before('\n}')
}
