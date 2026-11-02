module types

import os

fn test_selectively_imported_generic_struct_literals_infer_their_declared_type() {
	root := os.join_path(os.vtmp_dir(), 'selective_generic_struct_init_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vectors'))!
	os.mkdir_all(os.join_path(root, 'consumer'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'selective_generic_struct_init' }\n")!
	os.write_file(os.join_path(root, 'vectors', 'vectors.v'), 'module vectors

pub struct Vec2[T] {
pub:
	x T
	y T
}

pub fn (v Vec2[T]) first() T {
	return v.x
}
')!
	os.write_file(os.join_path(root, 'consumer', 'consumer.v'), 'module consumer

import vectors as vmath { Vec2 }

pub struct Size {
pub:
	width f32
	height f32
}

pub fn (s &Size) selected() Vec2[f32] {
	return Vec2{x: s.width, y: s.height}
}

pub fn (s &Size) qualified() vmath.Vec2[f32] {
	return vmath.Vec2{x: s.width, y: s.height}
}

pub fn text_vector() Vec2[string] {
	return Vec2{x: "first", y: "second"}
}
')!
	os.write_file(os.join_path(root, 'main.v'), 'import consumer

struct Vec2[T] {
	value T
}

fn local_vector(value f32) Vec2[f32] {
	return Vec2{value: value}
}

fn main() {
	size := consumer.Size{width: 1.5, height: 2.75}
	selected := size.selected()
	qualified := size.qualified()
	assert selected.x == f32(1.5) && selected.y == f32(2.75)
	assert qualified == selected
	assert typeof(selected.x).name == "f32"
	assert typeof(selected).name == "vectors.Vec2[f32]"
	assert selected.first() == f32(1.5)
	text := consumer.text_vector()
	assert text.first() == "first" && text.y == "second"
	assert typeof(text).name == "vectors.Vec2[string]"
	local := local_vector(f32(3.5))
	assert local.value == f32(3.5)
	assert typeof(local).name == "Vec2[f32]"
}
')!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', @CCOMPILER, 'run',
		root])
	assert result.exit_code == 0, result.output
}
