// vtest build: !windows
module main

import os

// These tests run whole runs against local repositories. A fake `make` installs a fake `v` that
// writes snapshots with the markers check_snapshot needs, so no compiler is built.

const fake_makefile = 'all:\n\tcp fake_v.sh v\n\tchmod +x v\n'

const fake_v = '#!/bin/sh
# Stands in for `v -o <file> -cross [-os windows -cc msvc] cmd/v`, run at the checkout root.
out=
target=nix
while [ \$# -gt 0 ]; do
	case "\$1" in
	-o) out=\$2; shift ;;
	windows) target=windows ;;
	esac
	shift
done
{
	echo "// Turned ON custom defines: no_backtrace,cross"
	echo "#define CUSTOM_DEFINE_cross"
	if [ \$target = nix ]; then
		echo "typedef i32 __v_int;"
		echo "#define __V_HEADERLESS_LIBC 0"
	fi
	echo "// \$target"
	cat compiler.txt
	yes "// filler" | head -n 5001
} > "\$out"
'

struct Fixture {
	root       string
	v_upstream string
	vc_remote  string
	first      string
}

fn fx_git(args []string) string {
	mut command := ['git', '-c', 'user.name=Test', '-c', 'user.email=test@example.invalid', '-c',
		'init.defaultBranch=master', '-c', 'commit.gpgsign=false']
	command << args
	result := os.exec(command)
	assert result.exit_code == 0, '${args}: ${result.output}'
	return result.output.trim_space()
}

fn fx_commit(repo string, files map[string]string, message string) string {
	for path, content in files {
		full := os.join_path(repo, path)
		os.mkdir_all(os.dir(full)) or { panic(err) }
		os.write_file(full, content) or { panic(err) }
	}
	fx_git(['-C', repo, 'add', '--all'])
	fx_git(['-C', repo, 'commit', '--quiet', '-m', message])
	return fx_git(['-C', repo, 'rev-parse', 'HEAD'])
}

fn new_fixture(name string) Fixture {
	// Keep the user's Git configuration out of both the fixture and the runs.
	os.setenv('GIT_CONFIG_GLOBAL', os.path_devnull, true)
	os.setenv('GIT_CONFIG_NOSYSTEM', '1', true)
	root := os.join_path(os.vtmp_dir(), 'gen_vc_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	v_upstream := os.join_path(root, 'v-upstream')
	fx_git(['init', '--quiet', v_upstream])
	first := fx_commit(v_upstream, {
		'Makefile':     fake_makefile
		'fake_v.sh':    fake_v
		'compiler.txt': 'compiler 1\n'
		'README.md':    'V\n'
	}, 'first compiler')
	vc_work := os.join_path(root, 'vc-work')
	fx_git(['init', '--quiet', vc_work])
	fx_commit(vc_work, {
		'README.md': 'vc\n'
		'v.c':       'old\n'
		'v_win.c':   'old\n'
	}, '[v:master] ${first} - first compiler')
	vc_remote := os.join_path(root, 'vc.git')
	fx_git(['clone', '--quiet', '--bare', vc_work, vc_remote])
	return Fixture{
		root:       root
		v_upstream: v_upstream
		vc_remote:  vc_remote
		first:      first
	}
}

fn (f Fixture) runner(mode Mode, name string) Runner {
	return Runner{
		config: Config{
			work_dir:              os.join_path(f.root, name)
			v_dir:                 os.join_path(f.root, name, 'v')
			mode:                  mode
			v_url:                 f.v_upstream
			vc_url:                f.vc_remote
			push_user:             bot_name
			build_timeout_minutes: 5
			retry_failed_minutes:  60
		}
		log:    Log{
			echo: false
		}
	}
}

fn (f Fixture) vc_master() string {
	return fx_git(['-C', f.vc_remote, 'rev-parse', 'master'])
}

fn (f Fixture) vc_show(spec string) string {
	return fx_git(['-C', f.vc_remote, 'show', spec])
}

fn run_log(r Runner) string {
	return r.log.lines.join('\n')
}

fn test_dry_run_commits_locally_and_push_mode_publishes_once() {
	f := new_fixture('flow')
	defer {
		os.rmdir_all(f.root) or {}
	}
	initial := f.vc_master()
	target := fx_commit(f.v_upstream, {
		'compiler.txt': 'compiler 2\n'
	}, 'cgen: change the compiler (#2)')
	mut dry := f.runner(.dry_run, 'dry')
	planned := dry.run(false)
	assert planned.outcome == .would_publish, run_log(dry)
	assert planned.v_commit == target
	assert f.vc_master() == initial
	dry_vc := os.join_path(f.root, 'dry', 'vc.git')
	assert fx_git(['-C', dry_vc, 'log', '-1', '--format=%B', planned.vc_commit]) == '[v:master] ${target} - cgen: change the compiler (#2)'
	assert fx_git(['-C', dry_vc, 'log', '-1', '--format=%an <%ae> %P', planned.vc_commit]) == '${bot_name} <${bot_email}> ${initial}'
	assert dry.run(false).outcome == .unchanged
	mut push := f.runner(.push, 'push')
	published := push.run(false)
	assert published.outcome == .published, run_log(push)
	assert f.vc_master() == published.vc_commit
	assert f.vc_show('master^') == f.vc_show(initial)
	assert fx_git(['-C', f.vc_remote, 'ls-tree', '--name-only', 'master']) == 'README.md\nv.c\nv_win.c'
	assert f.vc_show('master:README.md') == 'vc'
	v_c := f.vc_show('master:v.c')
	v_win_c := f.vc_show('master:v_win.c')
	assert v_c.starts_with(commit_hash_line(target))
	assert v_win_c.starts_with(commit_hash_line(target))
	assert v_c.contains('\n// nix\ncompiler 2\n')
	assert v_win_c.contains('\n// windows\ncompiler 2\n')
	// The dry run and the published run built the same snapshot.
	assert fx_git(['-C', dry_vc, 'rev-parse', '${planned.vc_commit}^{tree}']) == fx_git([
		'-C',
		f.vc_remote,
		'rev-parse',
		'master^{tree}',
	])
	assert push.run(false).outcome == .up_to_date
	assert push.run(false).outcome == .unchanged
	// A lock update, docs, and workflows never cause a new snapshot.
	fx_commit(f.v_upstream, {
		'thirdparty/tccbin_automation/bootstrap/vc.lock':              'commit=x\n'
		'thirdparty/tccbin_automation/tests/workflow_contract_test.v': '// x\n'
		'.github/workflows/update_tccbin.yml':                         'x: 1\n'
		'README.md':                                                   'V 2\n'
	}, 'tccbin: update vc.lock')
	ignored := push.run(false)
	assert ignored.outcome == .ignored, run_log(push)
	assert f.vc_master() == published.vc_commit
	// The next compiler change is published on top.
	next := fx_commit(f.v_upstream, {
		'compiler.txt': 'compiler 3\n'
	}, 'checker: next')
	again := push.run(false)
	assert again.outcome == .published, run_log(push)
	assert f.vc_show('master:v.c').starts_with(commit_hash_line(next))
	assert fx_git(['-C', f.vc_remote, 'rev-list', '--count', 'master']) == '3'
}

fn test_dry_run_compares_with_the_published_snapshot_of_the_same_commit() {
	f := new_fixture('verify')
	defer {
		os.rmdir_all(f.root) or {}
	}
	target := fx_commit(f.v_upstream, {
		'compiler.txt': 'compiler 2\n'
	}, 'cgen: change the compiler')
	mut push := f.runner(.push, 'push')
	assert push.run(false).outcome == .published, run_log(push)
	mut dry := f.runner(.dry_run, 'dry')
	verified := dry.run(false)
	assert verified.outcome == .verified, run_log(dry)
	assert dry.run(false).outcome == .unchanged
	// A snapshot of the same commit published by hand, with other bytes in v.c only.
	hand := os.join_path(f.root, 'hand')
	fx_git(['clone', '--quiet', f.vc_remote, hand])
	v_win_c := os.read_file(os.join_path(hand, 'v_win.c')) or { panic(err) }
	fx_commit(hand, {
		'v.c': commit_hash_line(target) + '// by hand\n'
	}, '[v:master] ${target} - regenerate v.c')
	assert os.read_file(os.join_path(hand, 'v_win.c')) or { panic(err) } == v_win_c
	fx_git(['-C', hand, 'push', '--quiet', 'origin', 'HEAD:master'])
	differs := dry.run(false)
	assert differs.outcome == .differs, run_log(dry)
	assert differs.message.ends_with(' in v.c'), differs.message
}

fn test_publish_skips_a_snapshot_that_master_moved_past() {
	f := new_fixture('stale')
	defer {
		os.rmdir_all(f.root) or {}
	}
	initial := f.vc_master()
	target := fx_commit(f.v_upstream, {
		'compiler.txt': 'compiler 2\n'
	}, 'cgen: change the compiler')
	mut push := f.runner(.push, 'push')
	prepared := push.prepare(false) or { panic('${err}\n${run_log(push)}') }
	assert prepared.outcome == .would_publish
	assert prepared.v_commit == target
	newer := fx_commit(f.v_upstream, {
		'compiler.txt': 'compiler 3\n'
	}, 'cgen: change it again')
	stale := push.publish(prepared) or { panic('${err}\n${run_log(push)}') }
	assert stale.outcome == .stale
	assert stale.message.contains(newer)
	assert f.vc_master() == initial
	next := push.run(false)
	assert next.outcome == .published, run_log(push)
	assert next.v_commit == newer
	assert f.vc_show('master:v.c').starts_with(commit_hash_line(newer))
}

fn test_a_failed_build_waits_for_a_new_commit_or_the_retry_delay() {
	f := new_fixture('failed')
	defer {
		os.rmdir_all(f.root) or {}
	}
	initial := f.vc_master()
	fx_commit(f.v_upstream, {
		'Makefile': 'all:\n\tfalse\n'
	}, 'break the build')
	mut push := f.runner(.push, 'push')
	failed := push.run(false)
	assert failed.outcome == .failed
	assert failed.message.starts_with('building V failed'), failed.message
	waiting := push.run(false)
	assert waiting.outcome == .unchanged
	assert waiting.message.contains('failed at'), waiting.message
	assert push.run(true).outcome == .failed
	assert f.vc_master() == initial
	// Restoring the Makefile alone would leave nothing to regenerate since the published source.
	fixed := fx_commit(f.v_upstream, {
		'Makefile':     fake_makefile
		'compiler.txt': 'compiler 2\n'
	}, 'fix the build')
	published := push.run(false)
	assert published.outcome == .published, run_log(push)
	assert published.v_commit == fixed
}
