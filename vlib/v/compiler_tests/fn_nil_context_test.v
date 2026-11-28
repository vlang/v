import json2
import os
import v.tempname

fn fn_nil_run_good(root string, name string, src string) string {
	good_src := os.join_path(root, 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(root, 'v3_${name}')
	compile := os.exec([@VEXE, '${good_src}', '-b', 'c', '-o', good_bin])
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	run := os.exec([good_bin])
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
}

struct FnNilRejection {
	name     string
	source   string
	expected string
}

struct FnNilDiagnostic {
	file     string
	severity string
	message  string
}

fn fn_nil_check_rejections(root string, cases []FnNilRejection) {
	project := os.join_path(root, 'rejections')
	mut imports := []string{cap: cases.len}
	for i, case in cases {
		module_name := 'fixture_${i}'
		imports << 'import ${module_name} as _'
		module_dir := os.join_path(project, module_name)
		os.mkdir_all(module_dir) or { panic(err) }
		os.write_file(os.join_path(module_dir, '${case.name}.v'), 'module ${module_name}\n\n' +
			case.source) or { panic(err) }
	}
	os.write_file(os.join_path(project, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n') or {
		panic(err)
	}
	result := os.exec([@VEXE, project, '-json-errors', '-b', 'c', '-o',
		os.join_path(project, 'program')])
	assert result.exit_code != 0, 'expected compile failure\n${result.output}'
	assert !result.output.contains('C compilation failed'), result.output
	mut errors := map[string][]string{}
	for line in result.output.split_into_lines() {
		if !line.starts_with('{') {
			continue
		}
		diagnostic := json2.decode[FnNilDiagnostic](line) or { panic(err) }
		if diagnostic.severity == 'error' {
			errors[os.file_name(diagnostic.file)] << diagnostic.message
		}
	}
	for case in cases {
		assert errors['${case.name}.v'].any(it.contains(case.expected)), '${case.name}: missing `${case.expected}` in\n${result.output}'
	}
}

fn test_unsafe_nil_contextually_matches_fn_fields() {
	root := os.join_path(os.vtmp_dir(), 'v3_fn_nil_context_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	out := fn_nil_run_good(root, 'fn_nil_fields', 'struct DefaultField {
mut:
	f fn (int) ! = unsafe { nil }
}

type Callback = fn (data voidptr)

struct AliasField {
	f Callback
}

fn main() {
	mut value := DefaultField{}
	value.f = unsafe { nil }
	println(int_str(1))

	_ := AliasField{
		f: unsafe { nil }
	}
	println(int_str(2))
}
')
	assert out == '1\n2'
	fn_nil_check_rejections(root, [
		FnNilRejection{
			name:     'fn_field_rejects_voidptr_variable'
			source:   'struct S {
	f fn (int) !
}

fn main() {
	v := unsafe { nil }
	_ := S{
		f: v
	}
}
'
			expected: 'cannot assign to field `f`: expected `fn(int) !void`, not `voidptr`'
		},
		FnNilRejection{
			name:     'fn_field_assignment_rejects_voidptr_variable'
			source:   'struct S {
	mut:
		f fn (int) !
}

fn main() {
	mut value := S{}
	v := unsafe { nil }
	value.f = v
}
'
			expected: 'cannot assign to `value.f`: expected `fn (int) !void`, not `voidptr`'
		},
		FnNilRejection{
			name:     'fn_field_rejects_non_nil_unsafe_pointer'
			source:   'struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: unsafe { &int(0) }
	}
}
'
			expected: 'cannot assign to field `f`: expected `fn(int) !void`, not `&int`'
		},
		FnNilRejection{
			name:     'fn_field_rejects_voidptr_zero'
			source:   'struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: voidptr(0)
	}
}
'
			expected: 'cannot assign to field `f`: expected `fn(int) !void`, not `voidptr`'
		},
		FnNilRejection{
			name:     'fn_field_rejects_integer_zero'
			source:   'struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: 0
	}
}
'
			expected: 'cannot assign to field `f`: expected `fn(int) !void`, not `int literal`'
		},
		FnNilRejection{
			name:     'fn_field_rejects_wrong_arity'
			source:   'fn no_args() ! {
	return
}

struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: no_args
	}
}
'
			expected: 'cannot assign to field `f`: expected `fn (int) !`, not `fn () !`'
		},
		FnNilRejection{
			name:     'fn_field_rejects_wrong_return'
			source:   'fn returns_int(i int) !int {
	return i
}

struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: returns_int
	}
}
'
			expected: 'cannot assign to field `f`: expected `fn (int) !`, not `fn (int) !int`'
		},
		FnNilRejection{
			name:     'fn_alias_field_rejects_wrong_arity'
			source:   'type Callback = fn (int) !

fn no_args() ! {
	return
}

struct S {
	f Callback
}

fn main() {
	_ := S{
		f: no_args
	}
}
'
			expected: 'cannot assign to field `f`: expected `fn (int) !`, not `fn () !`'
		},
	])
}
