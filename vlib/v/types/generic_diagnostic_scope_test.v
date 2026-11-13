module types

import os
import v.flat
import v.parser
import v.pref

fn generic_diagnostic_scope_fixture(calls string) !&flat.FlatAst {
	root := os.join_path(os.vtmp_dir(), 'v3_generic_diagnostic_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_path := os.join_path(root, 'main.v')
	assets_path := os.join_path(root, 'assets.v')
	palette_path := os.join_path(root, 'palette.v')
	os.write_file(main_path, 'module main
import assets
import palette
struct Image {}
fn main() { ${calls} }
')!
	os.write_file(assets_path, 'module assets
import palette as choice
pub struct Image {}
pub fn allow[T](value T) T {
	_ := choice.Image{}
	\$if T is Image {} \$else { \$compile_error("expected assets.Image") }
	return value
}
pub fn allow_param[T](value T) T {
	\$if value is Image {} \$else { \$compile_error("expected assets.Image parameter") }
	return value
}
pub fn imported[T](value T) T {
	\$if T is choice.Image {} \$else { \$compile_error("expected palette.Image") }
	return value
}
pub fn warning[T](value T) {
	\$if T is Image { \$compile_warn("local image") }
	\$else { \$compile_warn("foreign image") }
}
')!
	os.write_file(palette_path, 'module palette
pub struct Image {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([main_path, assets_path, palette_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	return a
}

fn test_generic_diagnostics_resolve_declared_types_and_import_aliases() {
	a := generic_diagnostic_scope_fixture('
		_ := assets.allow(assets.Image{})
		_ := assets.allow[assets.Image](assets.Image{})
		_ := assets.allow_param(assets.Image{})
		_ := assets.imported(palette.Image{})
		_ := assets.imported[palette.Image](palette.Image{})
	')!
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_generic_diagnostics_keep_same_named_caller_types_distinct() {
	a := generic_diagnostic_scope_fixture('
		_ := assets.allow(Image{})
		_ := assets.allow[Image](Image{})
		_ := assets.allow_param(Image{})
		_ := assets.allow(palette.Image{})
		_ := assets.imported(assets.Image{})
	')!
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 5, tc.errors.str()
	assert tc.errors.filter(it.kind == .compile_error && it.msg == 'expected assets.Image').len == 3, tc.errors.str()
	assert tc.errors.any(it.kind == .compile_error && it.msg == 'expected assets.Image parameter'), tc.errors.str()
	assert tc.errors.any(it.kind == .compile_error && it.msg == 'expected palette.Image'), tc.errors.str()
}

fn test_generic_warnings_use_declaration_scope_for_both_branches() {
	for argument, expected in {
		'assets.Image{}': 'local image'
		'Image{}':        'foreign image'
	} {
		a := generic_diagnostic_scope_fixture('assets.warning(${argument})')!
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		tc.check_semantics()
		assert tc.errors.len == 0, tc.errors.str()
		warnings := tc.notices.filter(it.msg in ['local image', 'foreign image'])
		assert warnings.len == 1, warnings.str()
		assert warnings[0].msg == expected, warnings.str()
	}
}
