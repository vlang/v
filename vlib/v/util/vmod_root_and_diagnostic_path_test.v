module util

import os

// make_project writes a `v.mod` at `level` directories above `root` and returns
// the directory that holds it.
fn make_project(root string, level int) !string {
	mut project := root
	for _ in 0 .. level {
		project = os.parent_dir(project)
	}
	os.mkdir_all(project)!
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'probe' }\n")!
	return project
}

fn test_nearest_vmod_root_finds_the_manifest_at_or_above_the_path() {
	root := os.join_path(os.vtmp_dir(), 'util_vmod_root_${os.getpid()}', 'a', 'b')
	os.rmdir_all(os.parent_dir(os.parent_dir(root))) or {}
	os.mkdir_all(root)!
	project := make_project(root, 2)!
	defer {
		os.rmdir_all(project) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main\n')!
	// A directory, a file inside it, and the directory that holds the manifest
	// itself all resolve to the same project root.
	assert nearest_vmod_root(root) or { '' } == os.real_path(project)
	assert nearest_vmod_root(source) or { '' } == os.real_path(project)
	assert nearest_vmod_root(project) or { '' } == os.real_path(project)
}

fn test_nearest_vmod_root_falls_back_to_the_working_directory() {
	root := os.join_path(os.vtmp_dir(), 'util_vmod_root_cwd_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'probe' }\n")!
	old_wd := os.getwd()
	os.chdir(root) or { panic(err) }
	defer {
		os.chdir(old_wd) or {}
		os.rmdir_all(root) or {}
	}
	assert nearest_vmod_root('') or { '' } == os.real_path(root)
	// A relative path is resolved against the working directory first.
	inner := os.join_path(root, 'inner')
	os.mkdir_all(inner)!
	assert nearest_vmod_root('inner') or { '' } == os.real_path(root)
}

// A `.git`/`.hg`/`.svn` checkout or a `.v.mod.stop` marker is the edge of the
// project: a `v.mod` above it belongs to something else and must not be
// reported as this one's root.
fn test_nearest_vmod_root_stops_at_a_project_boundary() {
	for marker in project_boundary_markers {
		root := os.join_path(os.vtmp_dir(), 'util_vmod_stop_${os.getpid()}', marker.replace('.',
			'_'))
		os.rmdir_all(os.parent_dir(root)) or {}
		os.mkdir_all(os.join_path(root, 'src'))!
		project := make_project(root, 1)!
		defer {
			os.rmdir_all(project) or {}
		}
		os.write_file(os.join_path(root, marker), '')!
		assert is_project_boundary_dir(root), marker
		if vmod_root := nearest_vmod_root(os.join_path(root, 'src')) {
			assert false, 'alias escaped ${marker}: ${vmod_root}'
		}
	}
}

// Without any manifest and without a boundary there is simply no project root.
fn test_nearest_vmod_root_returns_none_when_no_manifest_survives_the_walk() {
	root := os.join_path(os.vtmp_dir(), 'util_vmod_none_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'src'))!
	// A marker directly in the directory asked about ends the walk immediately,
	// whatever the parents hold.
	os.write_file(os.join_path(root, 'src', project_boundary_markers[0]), '')!
	defer {
		os.rmdir_all(root) or {}
	}
	assert nearest_vmod_root(os.join_path(root, 'src')) == none
}

fn test_project_boundary_markers_name_the_vcs_dirs_and_the_stop_marker() {
	assert project_boundary_markers.len == 4
	assert '.git' in project_boundary_markers
	assert '.hg' in project_boundary_markers
	assert '.svn' in project_boundary_markers
}

fn test_is_project_boundary_dir_reports_any_marker() {
	root := os.join_path(os.vtmp_dir(), 'util_boundary_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'plain'))!
	os.mkdir_all(os.join_path(root, 'marked'))!
	defer {
		os.rmdir_all(root) or {}
	}
	assert !is_project_boundary_dir(os.join_path(root, 'plain'))
	for marker in project_boundary_markers {
		os.write_file(os.join_path(root, 'marked', marker), '')!
	}
	assert is_project_boundary_dir(os.join_path(root, 'marked'))
	// A path that does not exist carries no marker.
	assert !is_project_boundary_dir(os.join_path(root, 'nope'))
}

fn test_path_styled_for_error_messages_prefers_a_relative_path() {
	// Only what lives below the startup directory is made relative; anything
	// else keeps the absolute, forward-slashed form it is resolved to.
	path := os.join_path(os.getwd(), 'util.v')
	assert path_styled_for_error_messages(path) == 'util.v'
	outside := os.join_path(os.parent_dir(os.getwd()), 'other.v')
	assert path_styled_for_error_messages(outside) == os.real_path(outside).replace('\\', '/')
}

fn test_path_styled_for_error_messages_honors_an_absolute_request() {
	old_paths := os.getenv_opt('VERROR_PATHS')
	defer {
		if value := old_paths {
			os.setenv('VERROR_PATHS', value, true)
		} else {
			os.unsetenv('VERROR_PATHS')
		}
	}
	path := os.join_path(os.getwd(), 'util.v')
	absolute := os.real_path(path).replace('\\', '/')
	os.setenv('VERROR_PATHS', 'absolute', true)
	assert path_styled_for_error_messages(path) == absolute
	// Any other value is not an absolute request.
	os.setenv('VERROR_PATHS', 'relative', true)
	assert path_styled_for_error_messages(path) == 'util.v'
}
