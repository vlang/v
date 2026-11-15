module types

import os

fn c_alias_visibility_fixture(name string) string {
	root := os.join_path(os.vtmp_dir(), 'c_alias_visibility_${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'bridge')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_alias_visibility' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'native.h'), 'struct NativeValue { int value; int secret; };\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'bridge', 'bridge.c.v'), 'module bridge
#include "@VMODROOT/native.h"

struct C.NativeValue {
	secret int
pub mut:
	value int
}

pub type DirectValue = C.NativeValue
type Implementation = C.NativeValue
pub type Value = Implementation
pub type NestedValue = Value
pub type GenericValue[T] = Implementation
pub type GenericReexport = GenericValue[int]
type PrivateValue = C.NativeValue

struct PrivateV {
pub:
	value int
}

pub type VAlias = PrivateV

pub struct PublicV {
	secret int
pub:
	value int
}

pub type PublicVAlias = PublicV
') or { panic(err) }
	return root
}

fn test_public_c_struct_alias_initializers_work_across_modules() {
	root := c_alias_visibility_fixture('public')
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'import bridge as native

type LocalValue = native.Value

fn main() {
	direct := native.DirectValue{value: 3}
	assert direct.value == 3
	mut value := native.Value{value: 40}
	value.value += 2
	assert value.value == 42
	ptr := &native.Value{value: 7}
	assert ptr.value == 7
	nested := native.NestedValue{value: 9}
	assert nested.value == 9
	local := LocalValue{value: 11}
	assert local.value == 11
	generic := native.GenericValue[int]{value: 13}
	assert generic.value == 13
	reexport := native.GenericReexport{value: 15}
	assert reexport.value == 15
}
')!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', @CCOMPILER, 'run',
		root])
	assert result.exit_code == 0, result.output
}

fn test_c_struct_alias_visibility_preserves_private_v_fields() {
	root := c_alias_visibility_fixture('fields')
	defer { os.rmdir_all(root) or {} }
	for body in ['_ = bridge.PublicV{secret: 1}', 'value := bridge.PublicV{}; _ = value.secret',
		'_ = bridge.PublicVAlias{secret: 1}', 'value := bridge.PublicVAlias{}; _ = value.secret'] {
		os.write_file(os.join_path(root, 'main.v'), 'import bridge\nfn main() { ${body} }\n')!
		result := os.exec([@VEXE, '-new-compiler', '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('secret')
			&& (result.output.contains('private') || result.output.contains('not public')), result.output
	}
}

fn test_c_struct_alias_initialization_preserves_private_names() {
	root := c_alias_visibility_fixture('private')
	defer { os.rmdir_all(root) or {} }
	for type_name, private_name in {
		'bridge.PrivateValue': 'bridge.PrivateValue'
		'bridge.PrivateV':     'bridge.PrivateV'
		'bridge.VAlias':       'bridge.PrivateV'
		'C.NativeValue':       'C.NativeValue'
	} {
		os.write_file(os.join_path(root, 'main.v'), 'import bridge
fn main() {
	_ = bridge.Value{}
	_ = ${type_name}{value: 1}
}
')!
		result := os.exec([@VEXE, '-new-compiler', '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('type `${private_name}` is private'), result.output
	}
}

fn test_local_aliases_cannot_expose_foreign_private_c_types() {
	root := c_alias_visibility_fixture('local')
	defer { os.rmdir_all(root) or {} }
	for target in ['bridge.PrivateValue', 'C.NativeValue'] {
		os.write_file(os.join_path(root, 'main.v'), 'import bridge
type LocalValue = ${target}
fn main() {
	_ = bridge.Value{}
	_ = LocalValue{value: 1}
}
')!
		result := os.exec([@VEXE, '-new-compiler', '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('type `${target}` is private'), result.output
	}
}
