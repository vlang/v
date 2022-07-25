import os
import time
import term

const vexe = os.getenv('VEXE')

const vroot = os.dir(vexe)

fn test_vexe_exists() {
	assert vexe.len > 0
	assert os.is_file(vexe)
}

fn test_v_profile_works() ? {
	os.chdir(vroot) or {}
	folders_root := os.join_path(vroot, 'vlib/v/tests/run_project_folders')
	folder_names := os.ls(folders_root)?
	mut folder_paths := []string{}
	for fname in folder_names {
		folder_path := os.join_path(folders_root, fname)
		if os.is_dir(folder_path) {
			folder_paths << folder_path
		}
	}
	for folder_path in folder_paths {
		local_path := folder_path.replace(vroot + os.path_separator, '').replace('\\',
			'/')
		println('...........   v run $local_path/')
		t := time.ticks()
		res := os.execute('${os.quoted_path(vexe)} run ${os.quoted_path(folder_path)}')
		delta := time.ticks() - t
		// eprintln('res: $res')
		assert res.exit_code == 0
		assert res.output.len > 0
		assert res.output.contains('OK')
		term.clear_previous_line()
		println('${term.bold('OK')} in ${delta:4}ms  v run $local_path/')
	}
}
