import os

fn test_default_clone_qualifies_imported_generic_field_method() {
	root := os.join_path(os.vtmp_dir(), 'default_clone_imported_generic_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'leaf'))!
	os.mkdir_all(os.join_path(root, 'wrapper'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'default_clone_imported_generic' }")!
	os.write_file(os.join_path(root, 'leaf', 'leaf.v'), 'module leaf

pub struct Box[T] implements IClone, Drop {
pub mut:
	values []T
	clones &int
	drops &int
}

pub fn (value &Box[T]) clone() Box[T] {
	unsafe { *value.clones += 1 }
	return Box[T]{
		values: value.values.clone()
		clones: value.clones
		drops: value.drops
	}
}

pub fn (mut value Box[T]) drop() {
	unsafe {
		*value.drops += 1
		value.values.free()
	}
	value.values = []T{}
}
')!
	os.write_file(os.join_path(root, 'wrapper', 'wrapper.v'), 'module wrapper

import leaf

struct Payload implements IClone {
mut:
	value int
}

pub struct Config implements IClone {
mut:
	inner leaf.Box[Payload]
}

pub struct Holder implements IClone {
mut:
	config Config
}

pub fn new(clones &int, drops &int) Config {
	return Config{
		inner: leaf.Box[Payload]{
			values: [Payload{value: 7}]
			clones: clones
			drops: drops
		}
	}
}

fn Holder.new(config &Config) Holder {
	return Holder{config: config.clone()}
}

pub struct Factory[T] {
	token T
}

pub fn factory[T](token T) Factory[T] {
	return Factory[T]{token: token}
}

pub fn (factory &Factory[T]) build(config &Config) Holder {
	return Holder.new(config)
}

pub fn copy(config &Config) Config {
	return *config
}

pub fn (mut holder Holder) set(value int) {
	holder.config.inner.values[0].value = value
}

pub fn (config &Config) value() int {
	return config.inner.values[0].value
}

pub fn (holder &Holder) value() int {
	return holder.config.value()
}
')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import wrapper

fn main() {
	mut clones := 0
	mut drops := 0
	{
		original := wrapper.new(&clones, &drops)
		factory := wrapper.factory(1)
		{
			mut holder := factory.build(&original)
			assert clones == 1
			assert holder.value() == 7
			holder.set(9)
			assert holder.value() == 9
			assert original.value() == 7
		}
		assert drops == 1
		{
			copied := wrapper.copy(&original)
			assert clones == 2
			assert copied.value() == 7
		}
		assert drops == 2
		assert original.value() == 7
	}
	assert drops == 3
}
')!
	for mode in ['-no-parallel', ''] {
		output := os.join_path(root, 'program_${if mode.len > 0 { 'serial' } else { 'parallel' }}')
		compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -ownership -d ownership -cc clang ${mode} -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
		assert compile.exit_code == 0, '${mode}: ${compile.output}'
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, '${mode}: ${run.output}'
	}
}
