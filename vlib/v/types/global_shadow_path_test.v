module types

import os

fn test_shadow_path_is_within_accepts_filesystem_root_descendants() {
	root := $if windows { 'C:' + os.path_separator } $else { os.path_separator }
	descendant := root + 'project' + os.path_separator + 'helpers' + os.path_separator + 'helpers.v'
	assert shadow_path_is_within(descendant, descendant, root)
}
