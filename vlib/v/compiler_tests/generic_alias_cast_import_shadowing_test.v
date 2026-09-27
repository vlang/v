import os

fn test_generic_body_keeps_import_alias_cast_identity() {
	root := os.join_path(os.vtmp_dir(), 'generic_alias_import_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	for name in ['container', 'real', 'shadow'] {
		os.mkdir_all(os.join_path(root, name))!
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'generic_alias_import' }\n")!
	os.write_file(os.join_path(root, 'real', 'real.v'), 'module real
pub struct Context {
pub:
	other string
}
pub type Value = u32
pub type Handler[T, U] = fn (T, U) u32
')!
	os.write_file(os.join_path(root, 'shadow', 'shadow.v'), 'module shadow
pub type Value = u64
pub type Handler[T, U] = fn (T, U) u64
')!
	os.write_file(os.join_path(root, 'container', 'container.v'), 'module container
import real as shadow

pub struct Context {
pub:
	value int
}

pub struct Pair[T, U] {
pub:
	first T
	second U
}

pub fn width[T](_ T) int {
	casted := shadow.Value(1)
	return sizeof(casted)
}

pub fn callback_width[T](val T, raw voidptr) int {
	callback := shadow.Handler[T, Context](raw)
	result := callback(val, Context{value: 11})
	assert result == 7
	return sizeof(result)
}

pub fn nested_callback_width[T](val T, raw voidptr) int {
	callback := shadow.Handler[T, Pair[T, Context]](raw)
	result := callback(val, Pair[T, Context]{first: val, second: Context{value: 13}})
	assert result == 9
	return sizeof(result)
}
')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import container
import shadow

struct Context {
	message string
}

fn callback(ctx Context, other container.Context) u32 {
	assert ctx.message == "ok"
	assert other.value == 11
	return 7
}

fn nested_callback(ctx Context, other container.Pair[Context, container.Context]) u32 {
	assert ctx.message == "nested"
	assert other.first.message == "nested"
	assert other.second.value == 13
	return 9
}

fn main() {
	assert container.width[int](0) == 4
	assert sizeof(shadow.Value) == 8
	assert container.callback_width[Context](Context{message: "ok"}, voidptr(callback)) == 4
	assert container.nested_callback_width[Context](Context{message: "nested"}, voidptr(nested_callback)) == 4
	actual := shadow.Handler[Context, int](fn (ctx Context, extra int) u64 {
		return u64(ctx.message.len + extra)
	})
	assert actual(Context{message: "abc"}, 2) == 5
}
')!
	output := os.join_path(root, 'main' + $if windows { '.exe' } $else { '' })
	build := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-retry-compilation -no-parallel -cc ${os.quoted_path(@CCOMPILER)} -gc none -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
	assert build.exit_code == 0, build.output
	run := os.execute(os.quoted_path(output))
	assert run.exit_code == 0, run.output
}
