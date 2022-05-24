module builder

import os

pub fn (b &Builder) dump_c_options(all_args []string) {
	dump_list(b.pref.dump_c_flags, all_args)
}

pub fn (b &Builder) dump_modules(mods []string) {
	dump_list(b.pref.dump_modules, mods)
}

pub fn (b &Builder) dump_files(files []string) {
	dump_list(b.pref.dump_files, files)
}

fn dump_list(file_path string, list []string) {
	if file_path != '' {
		content := list.filter(it != '').join('\n') + '\n'
		if file_path == '-' {
			print(content)
		} else {
			os.write_file(file_path, content) or { panic(err) }
		}
	}
}
