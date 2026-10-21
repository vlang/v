module document

import os
import v.pref

const cross_test_files = {
	'plat.v':            '// File is an open file.\npub struct File {}\n\npub fn everywhere() {}\n\npub const version = 1\n'
	'plat_nix.c.v':      "pub const sep = '/'\n\npub fn (f File) close() {}\n\npub fn on_unix() {}\n\n@[flag]\npub enum Mode {\n\tread\n\twrite\n}\n"
	'plat_windows.c.v':  "pub const sep = '\\\\'\n\npub fn (f File) close() {}\n\npub fn on_windows() {}\n\n@[flag]\npub enum Mode {\n\tread\n\twrite\n}\n"
	'plat_linux.c.v':    'pub fn on_linux() {}\n'
	'flags_nix.c.v':     'pub const flag_a = C.FLAG_A\npub const flag_b = C.FLAG_B\n\n// Flags with a comment.\npub const flag_c = C.FLAG_C\npub const flag_d = C.FLAG_D\n\n// flag_e has a comment.\npub const flag_e = C.FLAG_E\n'
	'flags_windows.c.v': 'pub const flag_a = 0\npub const flag_b = 0\n'
	'plat_darwin.c.v':   'pub fn on_macos() {}\n'
	'plat_bsd.c.v':      'pub fn on_bsd() {}\n'
	'plat_android.c.v':  'pub fn on_android() {}\n'
	'fb_default.c.v':    'pub fn fallback() {}\n'
	'fb_linux.c.v':      'pub fn fallback() {}\n'
}

fn cross_test_module(name string) !string {
	dir := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir)!
	for file, source in cross_test_files {
		os.write_file(os.join_path(dir, file), 'module plat\n\n' + source)!
	}
	return dir
}

fn test_cross_documents_the_symbols_of_all_platforms_with_their_platforms() {
	dir := cross_test_module('vdoc_cross')!
	defer { os.rmdir_all(dir) or {} }
	d := generate(dir, true, true, .cross)!
	// The symbols of files that are compiled for all platforms have no platforms.
	assert d.contents['everywhere']!.platforms == []
	// A `_default` file is compiled where no other file replaces it.
	assert d.contents['fallback']!.platforms == []
	assert d.contents['on_linux']!.platforms == [.linux]
	assert d.contents['on_macos']!.platforms == [.macos]
	assert d.contents['on_windows']!.platforms == [.windows]
	assert d.contents['on_android']!.platforms == [.android, .termux]
	// The compiler builds `_bsd` files for macOS too.
	assert d.contents['on_bsd']!.platforms == [.macos, .freebsd, .openbsd, .netbsd, .dragonfly]
	plain := fn (name string) string {
		return name
	}
	assert platforms_note(d.contents['on_windows']!.platforms, plain) == 'Available on Windows only.'
	assert platforms_note(d.contents['on_unix']!.platforms, plain) == 'Available on Unix only.'
	assert platforms_note(d.contents['on_android']!.platforms, plain) == 'Available on Android and Termux only.'
	assert platforms_note(d.contents['on_bsd']!.platforms, plain) == 'Available on macOS, FreeBSD, OpenBSD, NetBSD and DragonFly BSD only.'
	assert platforms_note(cross_platforms.filter(it !in [.ios, .haiku]), plain) == 'Available on all platforms except iOS and Haiku.'
	assert platforms_note(d.contents['everywhere']!.platforms, plain) == ''
	// Each output format emphasizes the platform names in its own way.
	assert platforms_note([.windows], fn (name string) string {
		return '<strong>${name}</strong>'
	}) == 'Available on <strong>Windows</strong> only.'
}

fn test_cross_documents_a_symbol_of_several_platforms_once() {
	dir := cross_test_module('vdoc_cross_merge')!
	defer { os.rmdir_all(dir) or {} }
	d := generate(dir, true, true, .cross)!
	// `_nix` and `_windows` together are all platforms.
	file_methods := d.contents['File']!.children.filter(it.name == 'close')
	assert file_methods.len == 1
	assert file_methods[0].platforms == []
	// A constant with a different value on each platform is documented once for each value.
	consts := d.contents['Constants']!.children.filter(child_key(it) == 'sep')
	assert consts.map(it.content) == ["pub const sep = '/'", r"pub const sep = '\\'"]
	assert platforms_note(consts[0].platforms, fn (name string) string {
		return name
	}) == 'Available on Unix only.'
	assert consts[1].platforms == [.windows]
	mode := d.contents['Mode']!
	assert mode.platforms == []
	assert mode.children.filter(it.name == 'has').len == 1
	assert d.contents['Mode.from']!.platforms == []
	assert d.contents['Mode.zero']!.platforms == []
}

fn test_cross_groups_the_constants_of_the_same_platforms() {
	dir := cross_test_module('vdoc_cross_groups')!
	defer { os.rmdir_all(dir) or {} }
	d := generate(dir, true, true, .cross)!
	consts := d.contents['Constants']!.children.filter(child_key(it).starts_with('flag_'))
	// A constant with a doc comment starts a new group, which the constants after it join.
	assert consts.map(it.content) == [
		'pub const flag_a = C.FLAG_A\npub const flag_b = C.FLAG_B',
		'pub const flag_c = C.FLAG_C\npub const flag_d = C.FLAG_D',
		'pub const flag_e = C.FLAG_E',
		'pub const flag_a = 0\npub const flag_b = 0',
	]
	assert consts.map(it.comments.len) == [0, 1, 1, 0]
	unix := cross_platforms.len - 1
	assert consts.map(it.platforms.len) == [unix, unix, unix, 1]
}

fn test_cross_documents_each_different_declaration_of_a_symbol() {
	dir := os.join_path(os.vtmp_dir(), 'vdoc_cross_variants_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	files := {
		// The methods come before the types in these files.
		'a_nix.c.v':     'module v\n\npub fn (c Ctx) size() int {\n\treturn 0\n}\n'
		'a_windows.c.v': 'module v\n\npub fn (c Ctx) size() int {\n\treturn 0\n}\n'
		// Ctx has another field on Windows, and run() only differs in how it is written.
		'b_nix.c.v':     'module v\n\n// Ctx is a context.\npub struct Ctx {\npub:\n\tfd int // the file\n}\n\n@[inline]\npub fn run(mut s Ctx, n int) {}\n'
		'b_windows.c.v': 'module v\n\n// Ctx is a context.\npub struct Ctx {\npub:\n\tfd     int\n\thandle voidptr\n}\n\npub fn run(mut ctx Ctx, count int) {}\n'
	}
	for file, source in files {
		os.write_file(os.join_path(dir, file), source)!
	}
	d := generate(dir, true, true, .cross)!
	ctx := d.contents['Ctx']!
	assert ctx.kind == .struct
	assert ctx.platforms.len == cross_platforms.len - 1
	assert ctx.variants.len == 1
	assert ctx.variants[0].platforms == [.windows]
	assert ctx.variants[0].content.contains('handle voidptr')
	assert ctx.children.map(it.name) == ['size']
	assert ctx.children[0].platforms == []
	run := d.contents['run']!
	assert run.variants == []
	assert run.platforms == []
}

fn test_declaration_shape_leaves_out_what_does_not_change_a_declaration() {
	assert declaration_shape('@[inline]\npub fn run(mut s Server, n int) !') == declaration_shape('fn run(mut server Server, count int) !')
	assert declaration_shape('pub fn (mut s Sem) destroy()') != declaration_shape('pub fn (s Sem) destroy()')
	assert declaration_shape('struct A {\n\tx int // the x\n}') == declaration_shape('struct A {\n\tx   int\n}')
	assert declaration_shape('struct A {\n\tx int\n}') != declaration_shape('struct A {\n\tx u8\n}')
	assert without_parameter_names('fn (mut s Server) map[T](f fn (int) int, shared m Map) (int, string)') == 'fn (mut Server) map[T](fn (int) int, shared Map) (int, string)'
	assert without_parameter_names('fn f(int, string)') == 'fn f(int, string)'
	assert declaration_shape('union U {\n\ttype int\n}') == declaration_shape('union U {\n\t@type int\n}')
	assert without_line_comment("url string = 'https://vlang.io' // the site") == "url string = 'https://vlang.io' "
	assert declaration_shape("struct A {\n\turl string = 'https://a'\n}") != declaration_shape("struct A {\n\turl string = 'https://b'\n}")
	// An unused parameter is named `_`.
	assert declaration_shape('fn f(pid int, mut m Map)') == declaration_shape('fn f(_ int, mut _ Map)')
}

fn test_a_single_platform_has_no_platform_labels() {
	dir := cross_test_module('vdoc_cross_single')!
	defer { os.rmdir_all(dir) or {} }
	d := generate(dir, true, true, .windows)!
	assert 'on_windows' in d.contents
	assert 'on_linux' !in d.contents
	for _, node in d.contents {
		assert node.platforms == []
	}
}

fn test_child_key_of_a_constant_is_its_name() {
	assert child_key(DocNode{
		kind:    .const_group
		content: "pub const sep = '/'"
	}) == 'sep'
	assert child_key(DocNode{
		kind:    .const_group
		content: '@[markused]\nconst max = 10'
	}) == 'max'
	assert child_key(DocNode{
		name: 'close'
		kind: .method
	}) == 'close'
}

fn test_the_compiler_supports_all_cross_platforms() {
	// The files of a platform that the compiler does not know would be the ones of the host.
	for platform in cross_platforms {
		pref.target_from('${platform}', 'amd64') or { assert false, '${platform}: ${err}' }
	}
}

fn test_a_doc_comment_on_one_platform_documents_the_symbol() {
	dir := os.join_path(os.vtmp_dir(), 'vdoc_cross_one_comment_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	os.write_file(os.join_path(dir, 'a_nix.c.v'), 'module a\n\npub fn run() {}\n')!
	os.write_file(os.join_path(dir, 'a_windows.c.v'), 'module a\n\n// run runs.\npub fn run() {}\n')!
	d := generate(dir, true, true, .cross)!
	assert d.contents['run']!.platforms == []
	assert d.contents['run']!.merge_comments().trim_space() == 'run runs.'
}

fn test_available_on_host() {
	host := host_platform()
	assert available_on_host(DocNode{})
	assert available_on_host(DocNode{
		platforms: [host]
	})
	other := if host == .windows { Platform.linux } else { Platform.windows }
	assert !available_on_host(DocNode{
		platforms: [other]
	})
	// A variant for the host makes the symbol available.
	assert available_on_host(DocNode{
		platforms: [other]
		variants:  [DocNode{
			platforms: [host]
		}]
	})
}

fn test_merge_platforms() {
	assert merge_platforms([.linux], [.windows]) == [.linux, .windows]
	assert merge_platforms([.windows], [.linux, .windows]) == [.linux, .windows]
	// No platforms means all of them.
	assert merge_platforms([], [.linux]) == []
	assert merge_platforms(cross_platforms.filter(it != .windows), [.windows]) == []
}
