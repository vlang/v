module file_lists

import os

// expand_files accepts a list of files and folders, and returns a list of all the .v and .vsh files, found in them.
// The input list of files, supports recursive `@file.lst` expansion, where each line is treated as another file/folder.
pub fn expand_files(files []string) ![]string {
	mut res := []string{}
	for file in files {
		if file == '' {
			continue
		}
		if file.starts_with('@') {
			lst_path := files[0].all_after('@').trim_space()
			listed_files := os.read_file(lst_path)!.split('\n').map(it.trim_space())
			res << expand_files(listed_files)!
			continue
		}
		if os.is_dir(file) {
			res << os.walk_ext(file, '.vsh')
			res << os.walk_ext(file, '.v')
			continue
		}
		if os.exists(file) {
			res << file
		}
	}
	return res
}
