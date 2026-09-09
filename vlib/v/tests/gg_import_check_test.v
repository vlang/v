import os

const issue_27584_vexe = os.quoted_path(@VEXE)

fn test_importing_gg_checks_cleanly_on_linux() {
	$if !linux {
		return
	}
	source_path := os.join_path(os.vtmp_dir(), 'issue_27584_import_gg_${os.getpid()}.v')
	defer {
		os.rm(source_path) or {}
	}
	os.write_file(source_path, 'module main

import gg as _

fn main() {}
') or { panic(err) }
	res := os.execute('${issue_27584_vexe} -os linux -check ${os.quoted_path(source_path)}')
	assert res.exit_code == 0, res.output
}
