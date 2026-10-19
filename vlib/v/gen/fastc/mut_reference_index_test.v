module fastc

import os
import v.cmdexec
import v.pref

// A `mut` reference local (`for mut word in words`, `if mut v is string`, a `mut`
// map parameter) is a C pointer to the value it names. Indexing it reads that value:
// a byte or a slice of the string, an entry of the map. It must not index the pointer
// as a C array (`((word)[1])`).
fn test_fastc_index_of_mut_reference_locals() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_mut_reference_index_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

type Name = string

type Value = int | string

fn word_parts(mut words []string) string {
	mut out := ""
	for mut word in words {
		first := word[0]
		out += first.ascii_str()
		out += word[1].ascii_str()
		part := word[0..2]
		out += part
		out += word[1..]
		if word[2] == `c` {
			out += "!"
		}
		word = word.to_upper()
	}
	return out
}

fn value_byte(mut v Value) u8 {
	if mut v is string {
		return v[0]
	}
	return 0
}

fn bump(mut uses map[string]int, name string) bool {
	uses[name]++
	uses[name]++
	uses[name]--
	return uses[name] == 1
}

fn main() {
	mut words := ["abc", "xyz"]
	assert word_parts(mut words) == "abab" + "bc!xyxyyz"
	assert words == ["ABC", "XYZ"]
	mut names := [Name("qwe")]
	for mut name in names {
		assert name[1] == `w`
		assert name[1..] == "we"
	}
	mut v := Value("hello")
	assert value_byte(mut v) == `h`
	mut uses := map[string]int{}
	assert bump(mut uses, "a")
	assert !bump(mut uses, "a")
	assert uses["a"] == 2
	println("ok")
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	for pointer_index in ['((word)[', '((v)[', '((name)[', '((uses)['] {
		assert !c_source.contains(pointer_index), pointer_index
	}
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
