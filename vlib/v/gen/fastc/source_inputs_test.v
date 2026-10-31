module fastc

import os
import v.pref

fn test_entry_expansion_keeps_lone_status_per_occurrence() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'fastc_entry_origins_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	file := os.join_path(root, 'a.v')
	os.write_file(file, 'module main\nfn main() {}\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for paths in [[root, file], [file, root]] {
		entries := fastc_expand_entry_paths(paths, prefs) or { panic(err) }
		assert entries.len == 2
		assert entries[0].path == file
		assert entries[1].path == file
		assert entries[0].lone == (paths[0] == file)
		assert entries[1].lone == (paths[1] == file)
	}
}

fn test_overlapping_directory_and_file_inputs_keep_module_siblings() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'fastc_overlapping_entries_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'a.v')
	helper_file := os.join_path(root, 'b.v')
	os.write_file(main_file, 'module main\nfn main() { println(answer()) }\n') or { panic(err) }
	os.write_file(helper_file, 'module main\nfn answer() int { return 42 }\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	// These expand to the same paths, so also exercise memo reuse across origins.
	for paths in [[root, main_file], [main_file, main_file], [main_file, root], [root, main_file]] {
		sources, _ := fastc_resolve_source_files(paths, prefs) or { panic(err) }
		assert sources.filter(it.path == main_file).len == 1
		assert sources.filter(it.path == helper_file).len == if root in paths { 1 } else { 0 }
	}
}

fn test_duplicate_explicit_program_inputs_still_exclude_siblings() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'fastc_duplicate_entries_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	file := os.join_path(root, 'a.v')
	sibling := os.join_path(root, 'b.v')
	os.write_file(file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(sibling, 'module main\nfn main() {}\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for paths in [[file], [file, file]] {
		sources, _ := fastc_resolve_source_files(paths, prefs) or { panic(err) }
		assert sources.filter(it.path == file).len == 1
		assert sources.filter(it.path == sibling).len == 0
	}
}
