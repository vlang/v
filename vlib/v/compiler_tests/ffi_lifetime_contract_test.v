import os

struct FfiLifetimeCase {
	name     string
	accepted bool
	source   string
}

fn test_ffi_lifetime_contracts_in_build_and_check() {
	root := os.join_path(os.vtmp_dir(), 'ffi_lifetime_contracts_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	cases := [
		FfiLifetimeCase{
			name:     'valid_pointer_args'
			accepted: true
			source:   "@[nonnull: 'p']\nfn C.check_ptr(p &u8) int\nfn main() { s := 'hello'\n\tn := C.check_ptr(s.str)\n\tprintln(n) }\n"
		}
		FfiLifetimeCase{
			name:     'checked_optional'
			accepted: true
			source:   "@[nonnull: 'p']\nfn C.check_ptr(p ?&u8) int\nfn maybe() ?&u8 {\n\treturn none\n}\nfn main() { s := 'hi'\n\tval := maybe()\n\tn := C.check_ptr(val or { s.str })\n\tprintln(n) }\n"
		}
		FfiLifetimeCase{
			name:     'unknown_unannotated'
			accepted: true
			source:   'fn C.other(p &u8) int\nfn main() { m := unsafe { C.other(nil) }\n\tprintln(m) }\n'
		}
		FfiLifetimeCase{
			name:     'unannotated_program'
			accepted: true
			source:   "fn main() { println('ok') }\n"
		}
		FfiLifetimeCase{
			name:     'callstyle'
			accepted: true
			source:   "@[nonnull('p')]\nfn C.check_ptr(p &u8) int\nfn main() { s := 'hi'\n\tn := C.check_ptr(s.str)\n\tprintln(n) }\n"
		}
		FfiLifetimeCase{
			name:     'comma_list'
			accepted: true
			source:   "@[nonnull: 'a, b']\nfn C.two(a &u8, b &u8) int\nfn main() { s := 'hi'\n\tn := C.two(s.str, s.str)\n\tprintln(n) }\n"
		}
		FfiLifetimeCase{
			name:     'len_of_valid'
			accepted: true
			source:   "@[len_of: 'buf=n']\nfn C.copy_data(buf &u8, n int) int\nfn main() { println(1) }\n"
		}
		FfiLifetimeCase{
			name:     'returns_owned_valid'
			accepted: true
			source:   "fn my_free(p voidptr) {\n\tunsafe { C.free(p) }\n}\n@[returns_owned: 'my_free']\nfn C.my_alloc(n int) voidptr\nfn main() { println(1) }\n"
		}
		FfiLifetimeCase{
			name:     'returns_borrowed_valid'
			accepted: true
			source:   "@[returns_borrowed: 'p']\nfn C.get_str(p &u8) &u8\nfn main() { println(1) }\n"
		}
		FfiLifetimeCase{
			name:     'method_valid'
			accepted: true
			source:   "struct Buf {\n\tmut:\n\t\tn int\n}\n@[nonnull: 'p']\nfn (b &Buf) write(p &u8) int {\n\treturn 0\n}\nfn main() { mut b := Buf{}\n\ts := 'hi'\n\tprintln(b.write(s.str)) }\n"
		}
		FfiLifetimeCase{
			name:     'nil_literal'
			accepted: false
			source:   "@[nonnull: 'p']\nfn C.check_ptr(p &u8) int\nfn main() { val := unsafe { C.check_ptr(nil) }\n\tprintln(val) }\n"
		}
		FfiLifetimeCase{
			name:     'unsafe_nil'
			accepted: false
			source:   "@[nonnull: 'p']\nfn C.check_ptr(p &u8) int\nfn main() { n := C.check_ptr(unsafe { nil })\n\tprintln(n) }\n"
		}
		FfiLifetimeCase{
			name:     'unchecked_optional'
			accepted: false
			source:   "@[nonnull: 'p']\nfn C.check_ptr(p ?&u8) int\nfn maybe() ?&u8 {\n\treturn none\n}\nfn main() { val := maybe()\n\tn := C.check_ptr(val)\n\tprintln(n) }\n"
		}
		FfiLifetimeCase{
			name:     'unknown_param'
			accepted: false
			source:   "@[nonnull: 'q']\nfn C.check_ptr(p &u8) int\nfn main() { println(1) }\n"
		}
		FfiLifetimeCase{
			name:     'bad_len_of'
			accepted: false
			source:   "@[len_of: 'buf']\nfn C.copy_data(buf &u8, n int) int\nfn main() { println(1) }\n"
		}
		FfiLifetimeCase{
			name:     'unknown_releaser'
			accepted: false
			source:   "@[returns_owned: 'C.free_value']\nfn C.my_alloc(n int) voidptr\nfn main() { println(1) }\n"
		}
		FfiLifetimeCase{
			name:     'noescape_nil'
			accepted: false
			source:   "@[noescape: 'p']\nfn C.use_ptr(p &u8) int\nfn main() { m := unsafe { C.use_ptr(nil) }\n\tprintln(m) }\n"
		}
		FfiLifetimeCase{
			name:     'non_fn_target'
			accepted: false
			source:   "@[nonnull: 'p']\nstruct Foo {\n\tn int\n}\nfn main() { println(1) }\n"
		}
		FfiLifetimeCase{
			name:     'unknown_capability'
			accepted: false
			source:   "@[nonnull_extra: 'p']\nfn C.check_ptr(p &u8) int\nfn main() { println(1) }\n"
		}
		FfiLifetimeCase{
			name:     'method_nil'
			accepted: false
			source:   "struct Buf {\n\tmut:\n\t\tn int\n}\n@[nonnull: 'p']\nfn (b &Buf) write(p &u8) int {\n\treturn 0\n}\nfn main() { mut b := Buf{}\n\tm := unsafe { b.write(nil) }\n\tprintln(m) }\n"
		},
	]
	// The standalone FastC parser must reject contracts it cannot check.
	fastc_source := os.join_path(root, 'fastc.v')
	os.write_file(fastc_source, "@[nonnull: 'p']\nfn handler(p &u8) int {\n\treturn 0\n}\nfn main() { s := 'hi'\n\tprintln(handler(s.str)) }\n") or {
		panic(err)
	}
	fastc_result := os.exec([@VEXE, '-b', 'fastc', '-o', os.join_path(root, 'fastc.c'), fastc_source])
	if !fastc_result.output.contains('fastc support is not compiled') {
		assert fastc_result.exit_code != 0 && fastc_result.output.contains('ffi'), fastc_result.output
	}

	for case in cases {
		source := os.join_path(root, '${case.name}.c.v')
		os.write_file(source, case.source) or { panic(err) }
		for mode in ['build', 'check'] {
			mut command := [@VEXE, '-gc', 'none']
			if mode == 'check' {
				command << '-check'
			} else {
				command << ['-o', os.join_path(root, '${case.name}.c')]
			}
			command << source
			result := os.exec(command)
			assert (result.exit_code == 0) == case.accepted, '${case.name} ${mode}: ${result.output}'
			if !case.accepted {
				assert result.output.contains('ffi'), '${case.name} ${mode}: ${result.output}'
			}
		}
	}
}

fn test_ffi_lifetime_cannot_reuse_an_unsafe_cached_dependency() {
	root := os.join_path(os.vtmp_dir(), 'ffi_lifetime_cached_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'ffidep')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'ffidep', 'ffidep.v'),
		"module ffidep\n@[nonnull: 'p']\npub fn C.check_ptr(p &u8) int\npub fn tag() string {\n\treturn 'ffidep'\n}\n") or {
		panic(err)
	}
	source := os.join_path(root, 'main.v')
	program := 'import ffidep\nfn main() {\n\tprintln(ffidep.tag())\n}\n'
	os.write_file(source, program) or { panic(err) }
	command := [@VEXE, '-gc', 'none', '-o', os.join_path(root, 'program.c'), source]
	warmup := os.exec(command)
	assert warmup.exit_code == 0, warmup.output
	os.write_file(source, program.replace('println(ffidep.tag())',
		'println(ffidep.tag())\n\tval := unsafe { C.check_ptr(nil) }\n\tprintln(val)')) or {
		panic(err)
	}
	for _ in 0 .. 2 {
		result := os.exec(command)
		assert result.exit_code != 0 && result.output.contains('ffi'), result.output
	}
}
