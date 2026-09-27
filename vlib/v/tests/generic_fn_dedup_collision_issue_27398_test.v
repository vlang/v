import os

fn write_file(path string, content string) {
	os.write_file(path, content) or { panic(err) }
}

fn test_generic_fn_dedup_collision_issue_27398() {
	n_pairs := 24
	tmp := os.join_path(os.vtmp_dir(), 'v_issue_27398_${os.getpid()}')
	os.rmdir_all(tmp) or {}
	defer {
		os.rmdir_all(tmp) or {}
	}

	os.mkdir_all(os.join_path(tmp, 'api')) or { panic(err) }
	write_file(os.join_path(tmp, 'api', 'common.v'), 'module api

pub struct MyStruct[T] {
pub:
	value T
}

pub fn make_struct[T](val T) MyStruct[T] {
	return MyStruct[T]{value: val}
}
')

	common_mod := 'module common

import api

pub struct Type_a { pub: name string }
pub struct Type_b { pub: age int }

pub fn call_api() {
	a := Type_a{name: "hello"}
	b := Type_b{age: 42}
	_ = api.make_struct(a)
	_ = api.make_struct(b)
}
'

	mut main_mod := 'module main\n\n'
	for i in 0 .. n_pairs {
		for _, s in ['a', 'b'] {
			dir := os.join_path(tmp, 'm${i}${s}', 'common')
			os.mkdir_all(dir) or { panic(err) }
			write_file(os.join_path(dir, 'types.v'), common_mod)
			main_mod += 'import m${i}${s}.common as m${i}${s}\n'
		}
	}
	main_mod += '\nfn main() {\n'
	for i in 0 .. n_pairs {
		for _, s in ['a', 'b'] {
			main_mod += '\tm${i}${s}.call_api()\n'
		}
	}
	main_mod += '\tprintln("ok")\n}\n'
	write_file(os.join_path(tmp, 'main.v'), main_mod)

	old_wd := os.getwd()
	os.chdir(tmp) or { panic(err) }
	defer {
		os.chdir(old_wd) or { panic(err) }
	}
	res := os.execute('${os.quoted_path(@VEXE)} -skip-unused run .')

	assert res.exit_code == 0, 'compilation failed:\n${res.output}'
	assert res.output.trim_space() == 'ok'
}
