module fastc

import os
import v.cmdexec
import v.pref

fn test_indexed_enum_collections_preserve_symbolic_printing() {
	source := "module main

enum Color { red green blue }
type Shade = Color
type Tone = Shade

fn main() {
	values := [Color.red, Color.blue]
	print(values[0])
	println(values[1])
	colors := {'primary': Color.green}
	print(colors['primary'])
	println(colors['missing'])
	shades := [Tone(Color.blue), Tone(Color.green)]
	print(shades[0])
	println(shades[1])
	tones := {'primary': Tone(Color.red)}
	print(tones['primary'])
	println(tones['missing'])
	println(Color.green)
}
"
	root := os.join_path(os.vtmp_dir(), 'fastc_enum_collections_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	mut prefs := pref.new_preferences()
	for selfhost in [false, true] {
		prefs.building_v = selfhost
		c_source := generate(source, 'indexed_enum_collections.v', prefs) or { panic(err) }
		if selfhost {
			// Every print still uses the enum printer, including aliases and missing map keys.
			assert c_source.all_after_last('int main(').count('v_fastc_print_enum_Color(') == 9, c_source
			continue
		}
		c_file := os.join_path(root, 'program.c')
		bin_file := os.join_path(root, 'program')
		os.write_file(c_file, c_source) or { panic(err) }
		tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
		compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(bin_file, [])
		assert run.exit_code == 0, run.output
		assert run.output == 'redblue\ngreenred\nbluegreen\nredred\ngreen\n', run.output
	}
}
