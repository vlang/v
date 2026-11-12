import os

fn test_explicit_callback_casts_keep_the_named_function_conversion() {
	root := os.join_path(os.vtmp_dir(), 'explicit_callback_cast_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, '0_globals.v'), '@[translated]
module main

@[markused]
@[weak] __global states = [State{callback: Callback{generic: Erased(voidptr(update_object))}},
	State{callback: Callback{generic: Erased(voidptr(update_pair))}}]!

fn main() {
	mut object := Object{value: 7}
	delta := Delta{value: 19}
	// A direct, typed callback still uses the original function.
	direct := Direct{callback: update_object}
	direct.callback(&object)
	assert object.value == 18
	assert voidptr(direct.callback) == voidptr(update_object)

	// Keep explicit conversions in a standalone value and local union field.
	standalone := Erased(voidptr(update_object))
	assert voidptr(standalone) == voidptr(update_object)
	ObjectCallback(voidptr(standalone))(&object)
	assert object.value == 29
	local := Callback{generic: Erased(voidptr(update_pair))}
	assert voidptr(local.generic) == voidptr(update_pair)
	local.pair(&object, &delta)
	assert object.value == 48

	// Translated C tables store generic pointers and call through typed members.
	assert voidptr(states[0].callback.generic) == voidptr(update_object)
	assert voidptr(states[1].callback.generic) == voidptr(update_pair)
	states[0].callback.object(&object)
	states[1].callback.pair(&object, &delta)
	assert object.value == 78
}
')!
	os.write_file(os.join_path(root, 'a_types.v'), '@[translated]
module main

type Action = fn ()
type ObjectCallback = fn (&Object)
union Callback {
	generic Action
	object ObjectCallback
	pair fn (&Object, &Delta)
}
struct Object {
mut:
	value i32
}
struct Delta {
	value i32
}
struct State {
	callback Callback
}
struct Direct {
	callback ObjectCallback
}
')!
	os.write_file(os.join_path(root, 'z_callbacks.v'), '@[translated]
module main

type Erased = fn ()

fn update_object(object &Object) {
	object.value += 11
}

fn update_pair(object &Object, delta &Delta) {
	object.value += delta.value
}
')!
	for flags in [[]string{}, ['-no-parallel', '-nocache']] {
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-translated', '-gc',
			'none', '-cc', @CCOMPILER, ...flags, 'run', root])
		assert result.exit_code == 0, result.output
	}
	c_path := os.join_path(root, 'callbacks.c')
	generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-translated', '-gc',
		'none', '-o', c_path, root])
	assert generated.exit_code == 0, generated.output
	c_source := os.read_file(c_path)!
	// GCC and TCC can accept the incompatible initializer as a warning. Check
	// that both global and local generic fields retain an actual conversion.
	assert !c_source.contains('.generic = update_object')
	assert !c_source.contains('.generic = update_pair')
	assert !c_source.contains('standalone = update_object')
	assert c_source.contains('.callback = update_object')
	if os.file_name(@CCOMPILER) in ['cc', 'gcc', 'clang'] {
		// Check the complete emitted C without translated-mode warning flags.
		mut strict_flags := []string{}
		if os.file_name(@CCOMPILER) == 'clang' {
			strict_flags << '-Werror=incompatible-function-pointer-types'
		} else if os.file_name(@CCOMPILER) == 'gcc' {
			strict_flags << '-Werror=incompatible-pointer-types'
		}
		checked := os.exec([@CCOMPILER, '-std=gnu11', ...strict_flags, '-fsyntax-only', c_path])
		assert checked.exit_code == 0, checked.output
	}
}
