module types

import os
import v.parser
import v.pref

fn check_enum_visibility_source(name string, imports string, body string) TypeChecker {
	root := os.join_path(os.vtmp_dir(), 'enum_visibility_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	main_path := os.join_path(root, 'main.v')
	colors_path := os.join_path(root, 'colors.v')
	os.write_file(main_path, '${imports}\nenum LocalColor { red }\nfn main() { ${body} }\n') or {
		panic(err)
	}
	os.write_file(colors_path, 'module colors
enum HiddenColor { red }
pub enum VisibleColor { red }
pub type ColorAlias = VisibleColor
pub fn own_private_color() bool { return HiddenColor.red == .red }
') or {
		panic(err)
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([main_path, colors_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc
}

fn test_imported_private_enum_members_are_rejected() {
	for qualifier in ['colors', 'palette'] {
		imports := if qualifier == 'colors' { 'import colors' } else { 'import colors as palette' }
		tc := check_enum_visibility_source('private_${qualifier}', imports,
			'_ = ${qualifier}.HiddenColor.red')
		assert tc.errors.len == 1, tc.errors.str()
		assert tc.errors[0].msg == 'enum `colors.HiddenColor` is private', tc.errors.str()
	}
}

fn test_public_and_same_module_enum_members_remain_valid() {
	tc := check_enum_visibility_source('public', 'import colors as palette', '
		assert LocalColor.red == .red
		assert palette.VisibleColor.red == .red
		assert palette.ColorAlias.red == .red
		assert palette.own_private_color()
	')
	assert tc.errors.len == 0, tc.errors.str()
}
