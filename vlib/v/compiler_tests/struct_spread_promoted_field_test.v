import os

fn spread_promoted_project(name string, files map[string]string, run bool) os.Result {
	root := os.join_path(os.vtmp_dir(), 'v_spread_promoted_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'spread_promoted' }\n") or {
		panic(err)
	}
	for relative, source in files {
		path := os.join_path(root, relative)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, source) or { panic(err) }
	}
	if !run {
		return os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-check', root])
	}
	binary := os.join_path(root, if os.user_os() == 'windows' { 'probe.exe' } else { 'probe' })
	compile := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-o', binary, root])
	if compile.exit_code != 0 {
		return compile
	}
	return os.exec([binary])
}

fn test_cross_struct_spread_accepts_complete_promoted_initializers() {
	result := spread_promoted_project('complete', {
		'support/support.v': 'module support
pub struct Engine {
	pub:
		tag int
}
pub struct Link {
	pub:
		engine &Engine
}
pub struct Nested {
	Link
	pub:
		active bool
}
'
		'main.v':            'module main
import support
struct LocalLink { token int }
struct LocalOwner {
	LocalLink
	width int
}
struct ImportedOwner {
	support.Link
	width int
}
struct NestedOwner {
	support.Nested
	width int
}
struct GenericLink[T] { value T }
struct GenericOwner[T] {
	GenericLink[T]
	width int
}
struct Config { width int }
fn main() {
	config := Config{width: 31}
	generic := GenericOwner[int]{...config, value: 17}
	assert generic.width == 31
	assert generic.value == 17
	local := LocalOwner{...config, token: 9}
	assert local.width == 31
	assert local.token == 9
	engine := support.Engine{tag: 7}
	imported := ImportedOwner{...config, engine: &engine}
	assert imported.width == 31
	assert imported.engine == &engine
	assert imported.engine.tag == 7
	nested := NestedOwner{...config, engine: &engine, active: true}
	assert nested.width == 31
	assert nested.engine == &engine
	assert nested.engine.tag == 7
	assert nested.active
	assert config.width == 31
}
'
	}, true)
	assert result.exit_code == 0, result.output
}

fn test_cross_struct_spread_requires_all_embedded_members() {
	result := spread_promoted_project('incomplete', {
		'main.v': 'struct Link {
	token int
	active bool
}
struct Owner {
	Link
	width int
}
struct Config { width int }
fn main() {
	config := Config{width: 31}
	_ = Owner{...config, token: 9}
}
'
	}, false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('struct `Config` is not compatible with struct `Owner`'), result.output
}

fn test_cross_struct_spread_checks_promoted_member_types() {
	result := spread_promoted_project('wrong_type', {
		'main.v': 'struct Link { token int }
struct Owner {
	Link
	width int
}
struct Config { width int }
fn main() {
	config := Config{width: 31}
	_ = Owner{...config, token: "wrong"}
}
'
	}, false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot assign to field `token`: expected `int`, not `string`'), result.output
}

fn test_cross_struct_spread_checks_concrete_generic_promoted_types() {
	result := spread_promoted_project('generic_wrong_type', {
		'main.v': 'struct Link[T] { value T }
struct Owner[T] {
	Link[T]
	width int
}
struct Config { width int }
fn main() {
	config := Config{width: 31}
	_ = Owner[int]{...config, value: "wrong"}
}
'
	}, false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot assign to field `value`: expected `int`, not `string`'), result.output
}

fn test_cross_struct_spread_does_not_count_shadowed_promoted_names() {
	for name, members in {
		'direct':        'Link\n\ttoken int'
		'nested':        'Middle'
		'duplicate':     'Link\n\tOther'
		'repeated_path': 'One\n\tAnother'
	} {
		result := spread_promoted_project(name, {
			'main.v': 'struct Link { token int }
struct Other { token int }
struct Middle {
	Link
	token int
}
struct One { Link }
struct Another { Link }
struct Owner {
	${members}
	width int
}
struct Config { width int }
fn main() {
	config := Config{width: 31}
	_ = Owner{...config, token: 9}
}
'
		}, false)
		assert result.exit_code != 0, '${name}: ${result.output}'
		assert result.output.contains('struct `Config` is not compatible with struct `Owner`'), '${name}: ${result.output}'
	}
}

fn test_cross_struct_spread_preserves_same_type_embed_copies() {
	result := spread_promoted_project('same_type', {
		'main.v': 'struct Link { token int }
struct Owner {
	Link
	width int
}
fn main() {
	base := Owner{token: 9, width: 31}
	copy := Owner{...base, width: 42}
	assert copy.token == 9
	assert copy.width == 42
	assert base.width == 31
}
'
	}, true)
	assert result.exit_code == 0, result.output
}
