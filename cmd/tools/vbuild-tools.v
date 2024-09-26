module main

import os
import testing
import v.util

// Note: tools like vdoc are compiled in their own subfolder
// => cmd/tools/vdoc/vdoc.exe
// Usually, they have several top level .v files in the subfolder,
// that cannot be compiled separately, but instead, the whole folder,
// should be compiled (v folder).
// To implement that, these folders are initially skipped, then added
// as a whole *after the testing.prepare_test_session call*.
const tools_in_subfolders = ['vast', 'vcreate', 'vdoc', 'vpm', 'vsymlink', 'vvet', 'vwhere', 'vcover']

// non_packaged_tools are tools that should not be packaged with
// prebuild versions of V, to keep the size smaller.
// They are mainly useful for the V project itself, not to end users.
const non_packaged_tools = ['gen1m', 'gen_vc', 'fast', 'wyhash']

fn main() {
	if os.getenv('VTEST_SANDBOXED_PACKAGING') == '' {
		util.ensure_modules_for_all_tools_are_installed('-v' in os.args)
	}
	args_string := os.args[1..].join(' ')
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)!
	folder := os.join_path('cmd', 'tools')
	tfolder := os.join_path(vroot, 'cmd', 'tools')
	main_label := 'Building ${folder} ...'
	finish_label := 'building ${folder}'

	mut skips := []string{}
	for stool in tools_in_subfolders {
		skips << os.join_path(tfolder, stool).replace('\\', '/')
	}
	buildopts := args_string.all_before('build-tools')
	mut session := testing.prepare_test_session(buildopts, folder, skips, main_label)
	session.rm_binaries = false
	session.build_tools = true
	for stool in tools_in_subfolders {
		session.add(os.join_path(tfolder, stool))
	}
	// eprintln('> session.files: $session.files')
	// eprintln('> session.skip_files: $session.skip_files')
	session.test()
	eprintln(session.benchmark.total_message(finish_label))
	if session.failed_cmds.len > 0 {
		exit(1)
	}

	mut executables := os.ls(session.vtmp_dir)!
	executables.sort()
	for texe in executables {
		tname := texe.replace(os.file_ext(texe), '')
		if tname in non_packaged_tools {
			continue
		}
		//
		tpath := os.join_path(session.vtmp_dir, texe)
		if texe.ends_with('_builder') || texe.ends_with('_builder.exe') {
			os.mv_by_cp(tpath, os.join_path(tfolder, 'builders', texe)) or { panic(err) }
			continue
		}
		if tname in tools_in_subfolders {
			os.mv_by_cp(tpath, os.join_path(tfolder, tname, texe)) or { panic(err) }
			continue
		}
		if os.is_dir(tpath) {
			continue
		}
		target_path := os.join_path(tfolder, texe)
		os.mv_by_cp(tpath, target_path) or {
			emsg := err.msg()
			if !emsg.contains('vbuild-tools') && !emsg.contains('vtest-all') {
				eprintln('error while moving ${tpath} to ${target_path}: ${emsg}')
			}
			continue
		}
	}
}
