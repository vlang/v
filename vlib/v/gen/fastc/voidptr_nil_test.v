module fastc

import os
import v.cmdexec
import v.pref

const voidptr_nil_source = r'module main

struct Handles {
mut:
	mapping voidptr
	file_handle voidptr
	typed &int = unsafe { nil }
}

fn (mut handles Handles) close() {
	handles.mapping = nil
	handles.file_handle = nil
	unsafe {
		handles.typed = nil
	}
}

fn main() {
	value := 42
	mut handles := Handles{
		mapping: voidptr(&value)
		file_handle: voidptr(&value)
		typed: &value
	}
	assert !isnil(handles.mapping)
	assert !isnil(handles.file_handle)
	assert *handles.typed == 42
	handles.close()
	assert isnil(handles.mapping)
	assert isnil(handles.file_handle)
	assert isnil(handles.typed)
	mut local := voidptr(&value)
	local = nil
	assert isnil(local)
	println("voidptr resets work")
}
'

fn test_fastc_voidptr_fields_and_locals_can_be_reset_to_nil() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate(voidptr_nil_source, 'voidptr_nil.v', prefs) or { panic(err) }
	assert c_source.contains('->mapping=NULL'), c_source
	assert c_source.contains('->file_handle=NULL'), c_source
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_voidptr_nil_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), voidptr_nil_source) or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'voidptr resets work', run.output
}

fn test_fastc_typed_pointer_nil_still_requires_unsafe() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for source in [
		'module main\nfn main() { value := 42; mut pointer := &value; pointer = nil }\n',
		'module main\nstruct Handles { mut: mapping voidptr }\nfn accept(pointer &int) voidptr { return voidptr(pointer) }\nfn main() { mut handles := Handles{}; handles.mapping = accept(nil) }\n',
	] {
		mut message := ''
		_ := generate(source, 'typed_pointer_nil.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('`nil` outside an `unsafe` block'), message
	}
}
