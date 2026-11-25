import os
import v.util.winpath

// What Windows makes of a path is spelled out here, not asked of V. A path that Windows makes none
// of, such as one longer than the 32767 characters a path can have, stays as it is, and so does every
// path on another system.
fn test_a_full_path_is_the_one_windows_makes() {
	long := 'C:\\' + 'a'.repeat(40000)
	assert winpath.full_path(long) == long
	$if windows {
		assert os.is_abs_path(winpath.absolute_path('a'.repeat(40000)))
	}
	for path, expected in {
		'C:\\x\\tools.':         'C:\\x\\tools'
		'C:\\x\\a\\..\\tools':   'C:\\x\\tools'
		'C:/x/tools':            'C:\\x\\tools'
		'C:\\x\\nul':            '\\\\.\\nul'
		'\\\\server\\share\\x.': '\\\\server\\share\\x'
	} {
		$if windows {
			assert winpath.full_path(path) == expected, path
			assert winpath.absolute_path(path) == expected, path
		} $else {
			assert winpath.full_path(path) == path, expected
			assert winpath.absolute_path(path) == path, expected
		}
	}
}

// A path that is no whole one is taken from the folder that is current, also when that is a link,
// which is not followed, with `..` as text.
fn test_a_path_is_taken_from_the_current_folder_and_not_from_where_it_leads() ! {
	$if !windows {
		return
	}
	root := os.join_path(os.abs_path(os.vtmp_dir()), 'winpath_test_${os.getpid()}')
	project := os.join_path(root, 'project')
	link := os.join_path(project, 'link')
	deep := os.join_path(root, 'elsewhere', 'deep')
	previous_dir := os.getwd()
	os.mkdir_all(project)!
	os.mkdir_all(deep)!
	defer {
		os.chdir(previous_dir) or {}
		// The junction itself, before what is around it.
		os.rmdir(link) or {}
		os.rmdir_all(root) or {}
	}
	junction := os.exec(['cmd', '/c', 'mklink', '/J', link, deep])
	assert junction.exit_code == 0, junction.output
	os.chdir(link)!
	drive := root[..2]
	assert drive[1] == `:`, root
	for path, expected in {
		'cache':             os.join_path_single(link, 'cache')
		'..\\cache':         os.join_path_single(project, 'cache')
		'${drive}cache':     os.join_path_single(link, 'cache')
		'${drive}..\\cache': os.join_path_single(project, 'cache')
		'${drive}':          link
		'\\cache':           '${drive}\\cache'
		'/cache':            '${drive}\\cache'
	} {
		assert winpath.full_path(path) == expected, path
		assert winpath.absolute_path(path) == expected, path
	}
}
