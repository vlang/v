import os

fn test_vml_generated_diagnostics_do_not_use_enclosing_source_positions() {
	root := os.join_path(os.vtmp_dir(), 'vml_generated_positions_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	module_dir := os.join_path(root, 'vmodules')
	os.mkdir_all(os.join_path(module_dir, 'ui2')) or { panic(err) }
	os.write_file(os.join_path(module_dir, 'ui2', 'ui2.v'), 'module ui2

pub struct Element {
pub mut:
	x int
	y int
	text string
}

pub fn bounds() Element {
	return Element{}
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'screen.vml'), 'Screen {
	id: root
	background: #FFFFFF
}
') or { panic(err) }
	source := os.join_path(root, 'main.v')
	os.write_file(source, "module main\n\nimport ui2\n\nfn make() ui2.Element {\n\treturn \$vml('screen.vml')\n}\n\nfn main() {\n\t_ = make()\n}\n") or {
		panic(err)
	}
	bin := os.join_path(root, 'bin')
	result := os.exec([@VEXE, '-nocache', '-gc', 'none', '-path', '@vlib|' + '${module_dir}', '-o',
		bin, source])
	assert result.exit_code != 0
	assert result.output.contains('<veb-template>:'), result.output
	assert result.output.contains('called from'), result.output
	assert result.output.contains('has no field named `width`'), result.output
}
