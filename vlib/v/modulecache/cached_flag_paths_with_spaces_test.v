module modulecache

import os
import v.util

// A V checkout whose path has a space in it (`/Volumes/Data 4TB/Developer/v`) wrote
// cached headers with `#flag /Volumes/Data /Volumes/Data 4TB/.../4TB/Developer/v/...`:
// the expanded path became two arguments, and the second one was resolved again,
// relative to the directory of the header (vlang/v#30633).

const spaced_vroot = '/Volumes/Data 4TB/Developer/v'

// header_flag_args splits a cached `#flag` value into arguments the way that the
// C flag parsers do when they read the header again.
fn header_flag_args(value string) []string {
	return os.split_args(value) or { panic(err) }
}

fn spaced_flag(value string, vroot string) string {
	return cached_directive_value('flag', value, vroot, os.join_path(vroot, 'vlib', 'builtin',
		'builtin_d_gcboehm.c.v'))
}

fn test_vexeroot_path_with_spaces_stays_one_argument() {
	value := spaced_flag('@VEXEROOT/vlib/builtin/gc_boehm_helpers.o', spaced_vroot)
	assert header_flag_args(value) == ['${spaced_vroot}/vlib/builtin/gc_boehm_helpers.o']
	tokens, valid := cached_split_flag_tokens(value)
	assert valid
	assert tokens.len == 1
}

fn test_joined_include_and_library_paths_keep_their_option() {
	value := spaced_flag('-I@VEXEROOT/thirdparty/libgc/include -L@VEXEROOT/thirdparty/tcc/lib -lgc',
		spaced_vroot)
	assert header_flag_args(value) == ['-I${spaced_vroot}/thirdparty/libgc/include',
		'-L${spaced_vroot}/thirdparty/tcc/lib', '-lgc']
	// The checker finds `#insert` include directories in the tokens of the flag.
	tokens := util.tokenize_c_flag(value)
	assert tokens.len == 3
	assert tokens[0].starts_with('-I')
	assert tokens[0][2..].trim('"\'') == '${spaced_vroot}/thirdparty/libgc/include'
}

fn test_other_options_with_spaced_paths_stay_one_argument() {
	assert header_flag_args(spaced_flag('-Wl,-rpath,@VEXEROOT/thirdparty/tcc/lib', spaced_vroot)) == [
		'-Wl,-rpath,${spaced_vroot}/thirdparty/tcc/lib',
	]
	// A quoted path needs no other quotes.
	quoted := spaced_flag('-Wl,-rpath,"@VEXEROOT/thirdparty/tcc/lib"', spaced_vroot)
	assert quoted == '-Wl,-rpath,"${spaced_vroot}/thirdparty/tcc/lib"'
	assert header_flag_args(quoted) == ['-Wl,-rpath,${spaced_vroot}/thirdparty/tcc/lib']
}

fn test_quotes_and_backslashes_in_spaced_paths_survive() {
	quote_root := "/Volumes/Steve's 4TB/v"
	assert header_flag_args(spaced_flag('@VEXEROOT/vlib/builtin/gc_boehm_helpers.o', quote_root)) == [
		'${quote_root}/vlib/builtin/gc_boehm_helpers.o',
	]
	windows_object := r'C:\Users\John Doe\v\vlib\builtin\gc_boehm_helpers.o'
	assert header_flag_args(cached_quote_flag_arg(windows_object)) == [windows_object]
}

fn test_relative_and_module_root_paths_with_spaces_stay_one_argument() {
	root := os.join_path(os.vtmp_dir(), 'v3 modulecache 30633 ${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	project := os.join_path(root, 'my project')
	source_dir := os.join_path(project, 'src')
	os.mkdir_all(os.join_path(source_dir, 'lib'))!
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'spaced' }\n")!
	object := os.join_path(source_dir, 'lib', 'helper.o')
	os.write_file(object, '')!
	source_file := os.join_path(source_dir, 'main.v')
	os.write_file(source_file, 'module main\n')!
	assert header_flag_args(cached_directive_value('flag', './lib/helper.o', '', source_file)) == [
		os.real_path(object),
	]
	assert header_flag_args(cached_directive_value('flag', '-I @VMODROOT/include', '', source_file)) == [
		'-I',
		os.join_path(os.real_path(project), 'include'),
	]
	assert header_flag_args(cached_directive_value('flag', '@DIR/lib/helper.o', '', source_file)) == [
		os.join_path(os.real_path(source_dir), 'lib', 'helper.o'),
	]
}

fn test_flags_without_spaces_keep_their_text() {
	vroot := '/opt/v'
	assert spaced_flag('@VEXEROOT/vlib/builtin/gc_boehm_helpers.o', vroot) == '/opt/v/vlib/builtin/gc_boehm_helpers.o'
	assert spaced_flag('-I@VEXEROOT/thirdparty/libgc/include  -lgc', vroot) == '-I/opt/v/thirdparty/libgc/include -lgc'
	assert spaced_flag('-DGC_THREADS=1  -lgc', vroot) == '-DGC_THREADS=1  -lgc'
	selection := r"darwin -I$when_first_existing('/opt/local/include','/opt/homebrew/include')"
	assert spaced_flag(selection, vroot) == selection
}
