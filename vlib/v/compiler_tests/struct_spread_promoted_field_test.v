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
		return os.exec([@VEXE, '-no-retry-compilation', '-check', root])
	}
	binary := os.join_path(root, if os.user_os() == 'windows' { 'probe.exe' } else { 'probe' })
	compile := os.exec([@VEXE, '-no-retry-compilation', '-o', binary, root])
	if compile.exit_code != 0 {
		return compile
	}
	return os.exec([binary])
}

fn test_cross_struct_spread_runtime_fixtures() {
	result := spread_promoted_project('runtime', {
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
		'complete.v':        'module main
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
fn check_complete_promoted_initializers() {
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
		'same_type.v':       'struct CopyLink { token int }
struct CopyOwner {
	CopyLink
	width int
}
fn check_same_type_embed_copies() {
	base := CopyOwner{token: 9, width: 31}
	copy := CopyOwner{...base, width: 42}
	assert copy.token == 9
	assert copy.width == 42
	assert base.width == 31
}
'
		'evaluation.v':      'struct DefaultLink { token int = unused_default() }
struct DefaultOwner { DefaultLink }
struct PartialOwner {
	DefaultLink
	width int
	fixed [2]int
	dynamic []int
	label string
}
struct FlatOwner { token int = unused_default() }
struct DefaultConfig {
	width int
	fixed [2]int
	dynamic []int
	label string
}
struct Counter {
mut:
	calls int
	order int
}
fn unused_default() int {
	panic("overridden defaults must not run")
}
fn make_config(mut counter Counter) DefaultConfig {
	counter.calls++
	counter.order = counter.order * 10 + 1
	return DefaultConfig{width: 31, fixed: [4, 5]!, dynamic: [6, 7], label: "copied"}
}
fn make_token(mut counter Counter) int {
	counter.order = counter.order * 10 + 2
	return 9
}
fn check_spread_evaluation() {
	config := DefaultConfig{width: 31}
	if config.width < 0 { unused_default() }
	owner := DefaultOwner{...config, token: 9}
	assert owner.token == 9
	assert config.width == 31
	mut counter := Counter{}
	called := DefaultOwner{...make_config(mut counter), token: make_token(mut counter)}
	assert called.token == 9
	assert counter.calls == 1
	assert counter.order == 12
	counter = Counter{}
	heap := &DefaultOwner{...make_config(mut counter), token: make_token(mut counter)}
	assert heap.token == 9
	assert counter.calls == 1
	assert counter.order == 12
	counter = Counter{}
	flat := FlatOwner{...make_config(mut counter), token: make_token(mut counter)}
	assert flat.token == 9
	assert counter.calls == 1
	assert counter.order == 12
	counter = Counter{}
	partial := PartialOwner{...make_config(mut counter), token: make_token(mut counter)}
	assert partial.token == 9
	assert partial.width == 31
	assert partial.fixed == [4, 5]!
	assert partial.dynamic == [6, 7]
	assert partial.label == "copied"
	assert counter.calls == 1
	assert counter.order == 12
}
'
		'ampersand_embed.v': 'struct PointerLink { token int }
struct PointerOwner { &PointerLink }
struct PointerConfig { width int }
fn check_ampersand_embed() {
	config := PointerConfig{width: 31}
	owner := PointerOwner{...config, token: 9}
	assert owner.token == 9
}
'
		'main.v':            'fn main() {
	check_complete_promoted_initializers()
	check_same_type_embed_copies()
	check_spread_evaluation()
	check_ampersand_embed()
}
'
	}, true)
	assert result.exit_code == 0, result.output
}

fn test_cross_struct_spread_rejects_invalid_promoted_initializers() {
	mut files := {
		'incomplete.v':          'struct IncompleteLink {
	token int
	active bool
}
struct IncompleteOwner {
	IncompleteLink
	width int
}
fn check_incomplete() {
	config := Config{width: 31}
	_ = IncompleteOwner{...config, token: 9}
}
'
		'typed_token.v':         'struct TypedLink { token int }
struct TypedOwner {
	TypedLink
	width int
}
fn check_wrong_type() {
	config := Config{width: 31}
	_ = TypedOwner{...config, token: "wrong"}
}
'
		'typed_generic_value.v': 'struct GenericLink[T] { value T }
struct GenericOwner[T] {
	GenericLink[T]
	width int
}
fn check_generic_wrong_type() {
	config := Config{width: 31}
	_ = GenericOwner[int]{...config, value: "wrong"}
}
'
		'shadowed_links.v':      'struct Link { token int }
struct Other { token int }
struct Middle {
	Link
	token int
}
struct One { Link }
struct Another { Link }
'
	}
	mut checks := ['check_incomplete()', 'check_wrong_type()', 'check_generic_wrong_type()']
	mut expected := {
		'incomplete.v':          'struct `Config` is not compatible with struct `IncompleteOwner`'
		'typed_token.v':         'cannot assign to field `token`: expected `int`, not `string`'
		'typed_generic_value.v': 'cannot assign to field `value`: expected `int`, not `string`'
	}
	for name, members in {
		'direct':        'Link\n\ttoken int'
		'nested':        'Middle'
		'duplicate':     'Link\n\tOther'
		'repeated_path': 'One\n\tAnother'
	} {
		owner := '${name.snake_to_camel()}Owner'
		file := '${name}.v'
		files[file] = 'struct ${owner} {
	${members}
	width int
}
fn check_${name}() {
	config := Config{width: 31}
	_ = ${owner}{...config, token: 9}
}
'
		checks << 'check_${name}()'
		expected[file] = 'struct `Config` is not compatible with struct `${owner}`'
	}
	files['main.v'] = 'struct Config { width int }

fn main() {
	${checks.join('\n\t')}
}
'
	result := spread_promoted_project('rejections', files, false)
	assert result.exit_code != 0, result.output
	diagnostics := result.output.split_into_lines().filter(it.contains(': error: '))
	for file, message in expected {
		assert diagnostics.any(it.contains('${file}:') && it.contains(message)), 'expected ${message} in ${file}\n${result.output}'
	}
}
