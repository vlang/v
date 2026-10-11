module types

import os

fn static_instance_method_project(name string) !string {
	root := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'src', 'components'))!
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n")!
	return root
}

fn write_static_instance_methods(root string, static_first bool, static_public bool) ! {
	static_visibility := if static_public { 'pub ' } else { '' }
	instance_visibility := if static_public { '' } else { 'pub ' }
	static_method := '${static_visibility}fn MyComponent.draw(value int, label string) int {
	return value + label.len
}
'
	instance_method := '${instance_visibility}fn (w &MyComponent) draw(label string) string {
	return MyComponent.draw(w.value, label).str()
}
'
	methods := if static_first {
		static_method + instance_method
	} else {
		instance_method + static_method
	}
	os.write_file(os.join_path(root, 'src', 'components', 'component.v'), 'module components
pub struct MyComponent {
	value int
}
${methods}
pub fn draw_instance() string {
	return MyComponent{value: 10}.draw("instance")
}
')!
}

fn test_imported_static_method_keeps_its_own_signature_and_visibility() {
	root := static_instance_method_project('static_instance_method_public')!
	defer { os.rmdir_all(root) or {} }
	for static_first in [false, true] {
		write_static_instance_methods(root, static_first, true)!
		for import_line, type_name in {
			'import src.components':                 'components.MyComponent'
			'import src.components as preview':      'preview.MyComponent'
			'import src.components { MyComponent }': 'MyComponent'
		} {
			module_name := if import_line.contains(' as ') { 'preview' } else { 'components' }
			os.write_file(os.join_path(root, 'main.v'), 'module main
${import_line}
fn main() {
	assert ${type_name}.draw(10, "preview") == 17
	draw := ${type_name}.draw
	assert draw(20, "callback") == 28
	assert ${module_name}.draw_instance() == "18"
	task := spawn ${type_name}.draw(30, "spawn")
	assert task.wait() == 35
}
')!
			for flags in [[]string{}, ['-no-parallel']] {
				result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', 'clang',
					'-gc', 'none', ...flags, 'run', root])
				assert result.exit_code == 0, result.output
			}
		}
	}
}

fn test_static_and_instance_method_privacy_remain_independent() {
	root := static_instance_method_project('static_instance_method_private')!
	defer { os.rmdir_all(root) or {} }
	for static_first in [false, true] {
		for static_public in [false, true] {
			write_static_instance_methods(root, static_first, static_public)!
			body := if static_public {
				'println(components.MyComponent{}.draw("private"))'
			} else {
				'println(components.MyComponent.draw(10, "private"))'
			}
			os.write_file(os.join_path(root, 'main.v'), 'module main
import src.components
fn main() {
	${body}
}
')!
			for flags in [[]string{}, ['-no-parallel']] {
				result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', 'clang',
					...flags, '-check', root])
				assert result.exit_code != 0, result.output
				kind := if static_public { 'method' } else { 'function' }
				assert result.output.contains('error: ${kind} `'), result.output
				assert result.output.contains('components.MyComponent.draw` is private'), result.output
			}
		}
	}
}
