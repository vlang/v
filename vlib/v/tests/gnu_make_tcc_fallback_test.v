import os
import rand

const makefile_path = os.join_path(@VEXEROOT, 'GNUmakefile')
const selector_path = os.join_path(@VEXEROOT, 'cmd', 'tools', 'select_linux_tcc.sh')
const git_argv_path = os.join_path(@VEXEROOT, 'cmd', 'tools', 'git_argv.sh')
const linux_tcc_build_script_path = os.join_path(@VEXEROOT, 'thirdparty', 'build_scripts',
	'thirdparty-linux-amd64_tcc.sh')
const linux_amd64_sysinclude_paths = '{B}/include:/usr/local/include/x86_64-linux-gnu:' +
	'/usr/local/include:/usr/include/x86_64-linux-gnu:/usr/include'

struct TccHistoryFixture {
	root             string
	remote           string
	source           string
	tmp_dir          string
	tcc_dir          string
	fresh_cmd        string
	latest_cmd       string
	compatible_sha   string
	incompatible_sha string
}

fn run_checked(command string) string {
	result := os.execute(command)
	assert result.exit_code == 0, 'command failed (${result.exit_code}):\n${command}\n${result.output}'
	return result.output
}

fn git_current_branch(repository string) string {
	command := 'git -C ${os.quoted_path(repository)} symbolic-ref --quiet --short HEAD'
	result := os.execute(command)
	assert result.exit_code == 0 || result.exit_code == 1, 'command failed (${result.exit_code}):\n${command}\n${result.output}'

	if result.exit_code == 1 {
		return ''
	}
	return result.output.trim_space()
}

fn write_executable(path string, contents string) {
	os.write_file(path, contents) or { panic(err) }
	os.chmod(path, 0o755) or { panic(err) }
}

fn run_linux_tcc_source_git_case(with_options bool) {
	root := os.join_path(os.vtmp_dir(), 'v_make_tcc_source_${rand.ulid()}')
	defer {
		os.rmdir_all(root) or {}
	}

	tcc_dir := os.join_path(root, 'thirdparty', 'tcc')
	tinycc_source := os.join_path(root, 'tinycc-source')
	bundle_workflow := os.join_path(tcc_dir, '.github', 'workflows', 'preserve.yml')
	source_workflow := os.join_path(tinycc_source, '.github', 'workflows', 'preserve.yml')
	bundle_workflow_contents := 'preserved bundle workflow\n'
	source_workflow_contents := 'source workflow must not replace the bundle workflow\n'
	bundle_gitignore_contents := 'preserved bundle ignore rules\n'
	bundle_gitattributes_contents := 'preserved bundle attributes\n'
	poison_bin := os.join_path(root, 'poison-bin')
	git_wrapper := os.join_path(root, 'custom-git')
	git_trace := os.join_path(root, 'git-trace')
	bare_git_trace := os.join_path(root, 'bare-git-trace')
	rsync_trace := os.join_path(root, 'rsync-trace')
	real_git := os.find_abs_path_of_executable('git') or { panic(err) }
	real_make := os.find_abs_path_of_executable('make') or { panic(err) }
	os.mkdir_all(os.join_path(root, 'vlib', 'v')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'cmd', 'tools')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'thirdparty', 'build_scripts')) or { panic(err) }
	os.mkdir_all(os.join_path(tcc_dir, 'lib')) or { panic(err) }
	os.mkdir_all(os.dir(bundle_workflow)) or { panic(err) }
	os.mkdir_all(poison_bin) or { panic(err) }
	os.write_file(os.join_path(root, 'vlib', 'v', 'compiler_errors_test.v'), '') or { panic(err) }
	os.symlink(makefile_path, os.join_path(root, 'GNUmakefile')) or { panic(err) }
	os.symlink(git_argv_path, os.join_path(root, 'cmd', 'tools', 'git_argv.sh')) or { panic(err) }
	os.symlink(linux_tcc_build_script_path, os.join_path(root, 'thirdparty', 'build_scripts',
		'thirdparty-linux-amd64_tcc.sh')) or { panic(err) }

	configure_source_repo(tcc_dir)
	write_executable(os.join_path(tcc_dir, 'tcc.exe'), '#!/bin/sh\necho old-tcc\n')
	os.write_file(os.join_path(tcc_dir, 'lib', 'libgc.a'), 'preserved-libgc\n') or { panic(err) }
	os.write_file(os.join_path(tcc_dir, 'lib', 'libgc_extra.a'), 'preserved-extra-libgc\n') or {
		panic(err)
	}
	os.write_file(os.join_path(tcc_dir, 'lib', 'build_libgc.sh'), 'preserved build helper\n') or {
		panic(err)
	}
	os.write_file(os.join_path(tcc_dir, 'lib', 'build_notes.txt'), 'preserved build notes\n') or {
		panic(err)
	}
	os.write_file(os.join_path(tcc_dir, 'README.md'), 'preserved readme\n') or { panic(err) }
	os.write_file(bundle_workflow, bundle_workflow_contents) or { panic(err) }
	os.write_file(os.join_path(tcc_dir, '.gitignore'), bundle_gitignore_contents) or { panic(err) }
	os.write_file(os.join_path(tcc_dir, '.gitattributes'), bundle_gitattributes_contents) or {
		panic(err)
	}
	run_checked('git -C ${os.quoted_path(tcc_dir)} add .')
	run_checked('git -C ${os.quoted_path(tcc_dir)} commit --quiet -m initial-bundle')

	configure_source_repo(tinycc_source)
	os.mkdir_all(os.dir(source_workflow)) or { panic(err) }
	os.write_file(source_workflow, source_workflow_contents) or { panic(err) }
	write_executable(os.join_path(tinycc_source, 'configure'), '#!/bin/sh
set -eu
prefix=
sysinclude_paths=
for arg in "\$@"; do
	case "\$arg" in
		--prefix=*) prefix="\${arg#--prefix=}" ;;
		--sysincludepaths=*) sysinclude_paths="\${arg#--sysincludepaths=}" ;;
	esac
done
test -n "\$prefix"
test -n "\$sysinclude_paths"
printf "%s\\n" "\$prefix" > .test-prefix
printf "%s\\n" "\$sysinclude_paths" > .test-sysincludepaths
')
	write_executable(os.join_path(tinycc_source, 'fake-tcc'), '#!/bin/sh
if [ "\${1:-}" = "--version" ]; then
	echo source-test-tcc
fi
exit 0
')
	os.write_file(os.join_path(tinycc_source, 'Makefile'), 'all:
	@:
install:
	@prefix="\$\$(cat .test-prefix)"; mkdir -p "\$\$prefix/lib/tcc/include" "\$\$prefix/.github/workflows"; cp fake-tcc "\$\$prefix/tcc"; cp .github/workflows/preserve.yml "\$\$prefix/.github/workflows/preserve.yml"; printf "source ignore rules\\n" > "\$\$prefix/.gitignore"; printf "source attributes\\n" > "\$\$prefix/.gitattributes"; printf "/* source test */\\n" > "\$\$prefix/lib/tcc/include/stddef.h"
') or {
		panic(err)
	}
	run_checked('git -C ${os.quoted_path(tinycc_source)} add .')
	run_checked('git -C ${os.quoted_path(tinycc_source)} commit --quiet -m fake-tinycc')
	tinycc_sha := run_checked('git -C ${os.quoted_path(tinycc_source)} rev-parse HEAD').trim_space()

	write_executable(git_wrapper, '#!/bin/sh
printf "%s\\n" "\$*" >> ${os.quoted_path(git_trace)}
exec ${os.quoted_path(real_git)} "\$@"
')
	write_executable(os.join_path(poison_bin, 'git'), '#!/bin/sh
echo bare-git >> ${os.quoted_path(bare_git_trace)}
exit 97
')
	write_executable(os.join_path(poison_bin, 'rsync'), '#!/bin/sh
set -eu
printf "%s\\n" "\$*" >> ${os.quoted_path(rsync_trace)}
archive=0
delete_destination=0
exclude_root_github=0
exclude_root_git=0
exclude_root_gitignore=0
exclude_root_gitattributes=0
while [ "\$#" -gt 0 ]; do
	case "\$1" in
		-a)
			archive=1
			shift
			;;
		--delete)
			delete_destination=1
			shift
			;;
		--exclude=/.github/)
			exclude_root_github=1
			shift
			;;
		--exclude=/.git|--exclude=/.git/)
			exclude_root_git=1
			shift
			;;
		--exclude=/.gitignore)
			exclude_root_gitignore=1
			shift
			;;
		--exclude=/.gitattributes)
			exclude_root_gitattributes=1
			shift
			;;
		--)
			shift
			break
			;;
		-*)
			echo "test rsync does not support option: \$1" >&2
			exit 98
			;;
		*)
			break
			;;
	esac
done
if [ "\$archive" != 1 ] || [ "\$#" -lt 2 ]; then
	echo "test rsync expects -a, at least one source, and one destination" >&2
	exit 98
fi
destination_path=
for argument in "\$@"; do
	destination_path=\$argument
done
source_count=\$((\$# - 1))
preserved_root_path=
if [ "\$delete_destination" = 1 ]; then
	destination_without_slash=\${destination_path%/}
	preserved_root_path="\${destination_without_slash}.rsync-test-preserved"
	rm -rf -- "\$preserved_root_path"
	mkdir -p -- "\$preserved_root_path"
	for entry_name in .git .github .gitignore .gitattributes; do
		preserve_entry=0
		case "\$entry_name" in
			.git) preserve_entry=\$exclude_root_git ;;
			.github) preserve_entry=\$exclude_root_github ;;
			.gitignore) preserve_entry=\$exclude_root_gitignore ;;
			.gitattributes) preserve_entry=\$exclude_root_gitattributes ;;
		esac
		if [ "\$preserve_entry" = 1 ] && { [ -e "\$destination_without_slash/\$entry_name" ] || [ -L "\$destination_without_slash/\$entry_name" ]; }; then
			mv -- "\$destination_without_slash/\$entry_name" "\$preserved_root_path/"
		fi
	done
	rm -rf -- "\$destination_path"
	mkdir -p -- "\$destination_path"
fi
source_index=1
for source_path in "\$@"; do
	if [ "\$source_index" -gt "\$source_count" ]; then
		break
	fi
	case "\$source_path" in
		*/)
			mkdir -p -- "\$destination_path"
			if [ "\$exclude_root_github" = 1 ] || [ "\$exclude_root_git" = 1 ] || [ "\$exclude_root_gitignore" = 1 ] || [ "\$exclude_root_gitattributes" = 1 ]; then
				for source_entry in "\${source_path}".[!.]* "\${source_path}"..?* "\${source_path}"*; do
					if [ ! -e "\$source_entry" ] && [ ! -L "\$source_entry" ]; then
						continue
					fi
					if { [ "\$exclude_root_github" = 1 ] && [ "\${source_entry##*/}" = ".github" ]; } || { [ "\$exclude_root_git" = 1 ] && [ "\${source_entry##*/}" = ".git" ]; } || { [ "\$exclude_root_gitignore" = 1 ] && [ "\${source_entry##*/}" = ".gitignore" ]; } || { [ "\$exclude_root_gitattributes" = 1 ] && [ "\${source_entry##*/}" = ".gitattributes" ]; }; then
						continue
					fi
					cp -a -- "\$source_entry" "\$destination_path/"
				done
			else
				cp -a -- "\${source_path}." "\$destination_path/"
			fi
			;;
		*)
			copy_into_directory=0
			if [ "\$source_count" -gt 1 ] || [ -d "\$destination_path" ]; then
				copy_into_directory=1
			else
				case "\$destination_path" in
					*/) copy_into_directory=1 ;;
				esac
			fi
			if [ "\$copy_into_directory" = 1 ]; then
				mkdir -p -- "\$destination_path"
				cp -a -- "\$source_path" "\$destination_path/"
			else
				destination_parent=\${destination_path%/*}
				if [ "\$destination_parent" != "\$destination_path" ]; then
					mkdir -p -- "\$destination_parent"
				fi
				cp -a -- "\$source_path" "\$destination_path"
			fi
			;;
	esac
	source_index=\$((source_index + 1))
done
if [ -n "\$preserved_root_path" ]; then
	for preserved_entry in "\$preserved_root_path"/.[!.]* "\$preserved_root_path"/..?*; do
		if [ ! -e "\$preserved_entry" ] && [ ! -L "\$preserved_entry" ]; then
			continue
		fi
		mv -- "\$preserved_entry" "\${destination_path%/}/"
	done
	rmdir -- "\$preserved_root_path"
fi
')
	git_options := if with_options { ' -c vlang.source-test=enabled' } else { '' }
	git_spec := '${git_wrapper}${git_options}'
	path := '${poison_bin}:/usr/bin:/bin'
	result :=
		os.execute('cd ${os.quoted_path(root)} && PATH=${os.quoted_path(path)} ${os.quoted_path(real_make)} --no-print-directory latest_tcc_source VROOT=. TCCOS=linux TCCARCH=amd64 TCC_COMMIT=${tinycc_sha} TCC_REPO=${os.quoted_path('file://${tinycc_source}')} GIT=${os.quoted_path(git_spec)} 2>&1')
	assert result.exit_code == 0, result.output
	assert !os.exists(bare_git_trace), result.output
	configured_sysinclude_paths := (os.read_file(os.join_path(root, 'tinycc',
		'.test-sysincludepaths')) or { panic(err) }).trim_space()
	assert configured_sysinclude_paths == linux_amd64_sysinclude_paths, configured_sysinclude_paths
	trace_contents := os.read_file(git_trace) or { panic(err) }
	trace_lines := trace_contents.trim_space().split_into_lines()
	assert trace_lines.len == 5, trace_lines.str()
	option_prefix := if with_options { '-c vlang.source-test=enabled ' } else { '' }
	for i, operation in ['clone ', 'checkout ', 'rev-parse ', 'add ', 'commit '] {
		assert trace_lines[i].starts_with('${option_prefix}${operation}'), trace_lines.str()
	}
	rsync_lines := (os.read_file(rsync_trace) or { panic(err) }).trim_space().split_into_lines()
	assert rsync_lines.len == 6, rsync_lines.str()
	assert rsync_lines[0].starts_with('-a --exclude=/.git --exclude=/.git/ thirdparty/tcc/ '), rsync_lines.str()
	assert rsync_lines[1].starts_with('-a --delete --exclude=/.git --exclude=/.git/ --exclude=/.github/ --exclude=/.gitignore --exclude=/.gitattributes '), rsync_lines.str()
	assert rsync_lines[2].contains('thirdparty/tcc.original/lib/libgc'), rsync_lines.str()
	assert rsync_lines[3].contains('thirdparty/tcc.original/lib/build'), rsync_lines.str()
	assert rsync_lines[4].contains('thirdparty/tcc.original/README.md'), rsync_lines.str()
	assert rsync_lines[5].ends_with('/build.sh'), rsync_lines.str()
	assert os.execute('${os.quoted_path(os.join_path(tcc_dir, 'tcc.exe'))} --version').output.trim_space() == 'source-test-tcc'
	staged_source_workflow := os.join_path(root, 'tinycc', 'thirdparty', 'tcc', '.github',
		'workflows', 'preserve.yml')
	staged_source_workflow_contents := os.read_file(staged_source_workflow) or { panic(err) }
	assert staged_source_workflow_contents == source_workflow_contents
	final_bundle_workflow_contents := os.read_file(bundle_workflow) or { panic(err) }
	assert final_bundle_workflow_contents == bundle_workflow_contents
	assert (os.read_file(os.join_path(tcc_dir, '.gitignore')) or { panic(err) }) == bundle_gitignore_contents
	assert (os.read_file(os.join_path(tcc_dir, '.gitattributes')) or { panic(err) }) == bundle_gitattributes_contents
	libgc := os.read_file(os.join_path(tcc_dir, 'lib', 'libgc.a')) or { panic(err) }
	assert libgc == 'preserved-libgc\n'
	extra_libgc := os.read_file(os.join_path(tcc_dir, 'lib', 'libgc_extra.a')) or { panic(err) }
	build_libgc := os.read_file(os.join_path(tcc_dir, 'lib', 'build_libgc.sh')) or { panic(err) }
	build_notes := os.read_file(os.join_path(tcc_dir, 'lib', 'build_notes.txt')) or { panic(err) }
	assert extra_libgc == 'preserved-extra-libgc\n'
	assert build_libgc == 'preserved build helper\n'
	assert build_notes == 'preserved build notes\n'
}

fn test_linux_tcc_source_uses_custom_git_argv_for_every_operation() {
	if os.user_os() != 'linux' {
		return
	}
	run_linux_tcc_source_git_case(false)
	run_linux_tcc_source_git_case(true)
}

fn tcc_probe_script_prefix(version string, includes_local bool) string {
	local_include_command := if includes_local { '\techo "  /usr/local/include"\n' } else { '' }
	return '#!/bin/sh
if [ "\${1:-}" = "--version" ]; then
	echo "${version}"
	exit 0
fi
if [ "\${1:-}" = "-print-search-dirs" ]; then
	echo "install: thirdparty/tcc/lib/tcc"
	echo "include:"
	echo "  thirdparty/tcc/lib/tcc/include"
	if [ -n "\${C_INCLUDE_PATH:-}" ]; then
		echo "  \$C_INCLUDE_PATH"
	fi
	if [ -n "\${CPATH:-}" ]; then
		echo "  \$CPATH"
	fi
${local_include_command}	echo "  /usr/include"
	echo "libraries:"
	echo "  thirdparty/tcc/lib/tcc"
	exit 0
fi
'
}

fn gc_compatible_tcc_script(version string, includes_local bool, requires_include_environment bool) string {
	include_environment_check := if requires_include_environment {
		'
if [ -z "\${C_INCLUDE_PATH:-}" ] && [ -z "\${CPATH:-}" ]; then
	echo "include path environment is unavailable" >&2
	exit 4
fi
'
	} else {
		''
	}
	return tcc_probe_script_prefix(version, includes_local) + include_environment_check +
		'
if [ ! -f thirdparty/tcc/lib/tcc/include/stddef.h ]; then
	echo "tcc: error: thirdparty/tcc/lib/tcc/include/stddef.h is unavailable from the current directory" >&2
	exit 3
fi
out=
has_gc_include=0
has_gc_threads=0
has_thread_local_alloc=0
has_builtin_atomic=0
has_libgc=0
while [ "\$#" -gt 0 ]; do
	case "\$1" in
		-I*/thirdparty/libgc/include) has_gc_include=1 ;;
		-DGC_THREADS=1) has_gc_threads=1 ;;
		-DTHREAD_LOCAL_ALLOC=1) has_thread_local_alloc=1 ;;
		-DGC_BUILTIN_ATOMIC=1) has_builtin_atomic=1 ;;
		*/lib/libgc.a) has_libgc=1 ;;
		-o)
			if [ "\$#" -gt 1 ]; then
				out="\$2"
				shift 2
				continue
			fi
			;;
	esac
	shift
done
if [ -z "\$out" ] || [ "\$has_gc_include" != 1 ] || [ "\$has_gc_threads" != 1 ] || [ "\$has_thread_local_alloc" != 1 ] || [ "\$has_builtin_atomic" != 1 ] || [ "\$has_libgc" != 1 ]; then
	exit 2
fi
{
	echo "#!/bin/sh"
	echo "echo v-tcc-host-boehm-probe"
} > "\$out"
chmod +x "\$out"
'
}

fn compatible_tcc_script(version string) string {
	return gc_compatible_tcc_script(version, true, false)
}

fn missing_local_include_tcc_script(version string) string {
	return gc_compatible_tcc_script(version, false, false)
}

fn include_environment_dependent_tcc_script(version string) string {
	return gc_compatible_tcc_script(version, true, true)
}

fn runtime_incompatible_tcc_script(version string) string {
	return tcc_probe_script_prefix(version, true) +
		'
out=
while [ "\$#" -gt 0 ]; do
	if [ "\$1" = "-o" ] && [ "\$#" -gt 1 ]; then
		out="\$2"
		shift 2
		continue
	fi
	shift
done
if [ -z "\$out" ]; then
	exit 2
fi
{
	echo "#!/bin/sh"
	echo "echo incompatible-host-probe"
	echo "exit 43"
} > "\$out"
chmod +x "\$out"
'
}

fn configure_source_repo(path string) {
	run_checked('git init --quiet ${os.quoted_path(path)}')
	run_checked('git -C ${os.quoted_path(path)} config user.name "V Test"')
	run_checked('git -C ${os.quoted_path(path)} config user.email "v-test@example.invalid"')
}

fn commit_bundle_state(source string, message string, tcc_script string, libgc string) string {
	os.mkdir_all(os.join_path(source, 'lib')) or { panic(err) }
	os.mkdir_all(os.join_path(source, 'lib', 'tcc', 'include')) or { panic(err) }
	write_executable(os.join_path(source, 'tcc.exe'), tcc_script)
	os.write_file(os.join_path(source, 'lib', 'libgc.a'), libgc) or { panic(err) }
	os.write_file(os.join_path(source, 'lib', 'tcc', 'include', 'stddef.h'),
		'/* bundle-relative TCC include contract */\n') or { panic(err) }
	os.write_file(os.join_path(source, 'lib', 'bundle-state.txt'), '${message}\n') or { panic(err) }
	run_checked('git -C ${os.quoted_path(source)} add .')
	run_checked('git -C ${os.quoted_path(source)} commit --quiet -m ${os.quoted_path(message)}')
	return run_checked('git -C ${os.quoted_path(source)} rev-parse HEAD').trim_space()
}

fn create_unknown_branch(root string, remote string) {
	source := os.join_path(root, 'unknown')
	configure_source_repo(source)
	run_checked('git -C ${os.quoted_path(source)} checkout --quiet -b thirdparty-unknown-unknown')
	write_executable(os.join_path(source, 'tcc.exe'),
		'#!/bin/sh\necho "no bundled tcc" >&2\nexit 1\n')
	run_checked('git -C ${os.quoted_path(source)} add tcc.exe')
	run_checked('git -C ${os.quoted_path(source)} commit --quiet -m unknown')
	run_checked('git -C ${os.quoted_path(source)} push --quiet ${os.quoted_path(remote)} HEAD:refs/heads/thirdparty-unknown-unknown')
}

fn create_musl_branch(root string, remote string) {
	source := os.join_path(root, 'musl')
	configure_source_repo(source)
	run_checked('git -C ${os.quoted_path(source)} checkout --quiet -b thirdparty-linuxmusl-amd64')
	commit_bundle_state(source, 'musl-bundle', compatible_tcc_script('musl-tcc'), 'musl-libgc\n')
	run_checked('git -C ${os.quoted_path(source)} push --quiet ${os.quoted_path(remote)} HEAD:refs/heads/thirdparty-linuxmusl-amd64')
}

fn new_tcc_history_fixture(with_compatible_ancestor bool) TccHistoryFixture {
	root := os.join_path(os.vtmp_dir(), 'v_make_tcc_history_${rand.ulid()}')
	remote := os.join_path(root, 'tccbin.git')
	source := os.join_path(root, 'linux-source')
	vroot := os.join_path(root, 'vroot')
	tmp_dir := os.join_path(root, 'tmp')
	tcc_dir := os.join_path(vroot, 'thirdparty', 'tcc')
	os.mkdir_all(os.join_path(vroot, 'thirdparty')) or { panic(err) }
	os.mkdir_all(os.join_path(vroot, 'thirdparty', 'libgc', 'include')) or { panic(err) }
	os.mkdir_all(os.join_path(vroot, 'cmd', 'tools')) or { panic(err) }
	os.mkdir_all(tmp_dir) or { panic(err) }
	os.write_file(os.join_path(vroot, 'thirdparty', 'libgc', 'include', 'gc.h'),
		'void GC_INIT(void);\nvoid *GC_MALLOC(unsigned long);\n') or { panic(err) }
	os.symlink(makefile_path, os.join_path(vroot, 'GNUmakefile')) or { panic(err) }
	os.symlink(selector_path, os.join_path(vroot, 'cmd', 'tools', 'select_linux_tcc.sh')) or {
		panic(err)
	}
	os.symlink(git_argv_path, os.join_path(vroot, 'cmd', 'tools', 'git_argv.sh')) or { panic(err) }
	run_checked('git init --quiet --bare ${os.quoted_path(remote)}')
	configure_source_repo(source)
	run_checked('git -C ${os.quoted_path(source)} checkout --quiet -b thirdparty-linux-amd64')
	mut compatible_sha := ''
	if with_compatible_ancestor {
		commit_bundle_state(source, 'compatible-v0', compatible_tcc_script('compatible-tcc-v0'),
			'compatible-libgc-v0\n')
		compatible_sha = commit_bundle_state(source, 'compatible-v1',
			compatible_tcc_script('compatible-tcc-v1'), 'compatible-libgc-v1\n')
		commit_bundle_state(source, 'runtime-incompatible',
			runtime_incompatible_tcc_script('runtime-incompatible-tcc'),
			'runtime-incompatible-libgc\n')
	}
	incompatible_sha := commit_bundle_state(source, 'include-environment-dependent',
		include_environment_dependent_tcc_script('include-environment-dependent-tcc'),
		'include-environment-dependent-libgc\n')
	run_checked('git -C ${os.quoted_path(source)} push --quiet ${os.quoted_path(remote)} HEAD:refs/heads/thirdparty-linux-amd64')
	create_unknown_branch(root, remote)
	create_musl_branch(root, remote)

	// Keep fixture intent independent from the compiler flags of the outer test lane.
	// Individual scenarios can still override VFLAGS after these default arguments.
	make_args := 'VROOT=${os.quoted_path(vroot)} TCCREPO=${os.quoted_path('file://${remote}')} TMPDIR=${os.quoted_path(tmp_dir)} TCCOS=linux TCCARCH=amd64 VFLAGS='
	fresh_cmd := 'cd ${os.quoted_path(vroot)} && make --no-print-directory fresh_tcc ${make_args}'
	latest_cmd := 'cd ${os.quoted_path(vroot)} && make --no-print-directory latest_tcc ${make_args}'
	return TccHistoryFixture{
		root:             root
		remote:           remote
		source:           source
		tmp_dir:          tmp_dir
		tcc_dir:          tcc_dir
		fresh_cmd:        fresh_cmd
		latest_cmd:       latest_cmd
		compatible_sha:   compatible_sha
		incompatible_sha: incompatible_sha
	}
}

fn compatible_marker_dir(fixture TccHistoryFixture) string {
	return os.join_path(fixture.tcc_dir, '.git', 'vlang-compatible-tcc')
}

fn assert_clean_checkout(tcc_dir string) {
	status := run_checked('git -C ${os.quoted_path(tcc_dir)} status --short')
	assert status.trim_space() == '', status
}

fn assert_historical_fallback(fixture TccHistoryFixture) {
	assert git_current_branch(fixture.tcc_dir) == ''
	assert run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space() == fixture.compatible_sha
	libgc := os.read_file(os.join_path(fixture.tcc_dir, 'lib', 'libgc.a')) or { panic(err) }
	metadata := os.read_file(os.join_path(compatible_marker_dir(fixture), 'metadata')) or {
		panic(err)
	}
	tmp_entries := os.ls(fixture.tmp_dir) or { panic(err) }
	assert libgc == 'compatible-libgc-v1\n'
	expected_metadata := 'tccos=linux\n' + 'tccarch=amd64\n' + 'abi=glibc\n' +
		'branch=thirdparty-linux-amd64\n' + 'remote_head_sha=${fixture.incompatible_sha}\n' +
		'compatible_sha=${fixture.compatible_sha}\n'
	assert metadata == expected_metadata, metadata
	assert_clean_checkout(fixture.tcc_dir)
	assert tmp_entries == []
}

fn assert_fixture_bundle_is_vroot_cwd_sensitive(fixture TccHistoryFixture) {
	vroot := os.dir(os.dir(fixture.tcc_dir))
	source_path := os.join_path(fixture.tmp_dir, 'wrong-cwd-probe.c')
	executable_path := os.join_path(fixture.tmp_dir, 'wrong-cwd-probe')
	os.write_file(source_path, 'int main(void) { return 0; }\n') or { panic(err) }
	result := os.execute('cd ${os.quoted_path(fixture.tcc_dir)} && ./tcc.exe -I${os.quoted_path(os.join_path(vroot,
		'thirdparty', 'libgc', 'include'))} -DGC_THREADS=1 -DTHREAD_LOCAL_ALLOC=1 -DGC_BUILTIN_ATOMIC=1 -o ${os.quoted_path(executable_path)} ${os.quoted_path(source_path)} ${os.quoted_path(os.join_path(fixture.tcc_dir,
		'lib', 'libgc.a'))} -ldl -lpthread 2>&1')
	assert result.exit_code != 0, result.output
	assert result.output.contains('thirdparty/tcc/lib/tcc/include/stddef.h is unavailable'), result.output

	os.rm(source_path) or {}
	os.rm(executable_path) or {}
	tmp_entries := os.ls(fixture.tmp_dir) or { panic(err) }
	assert tmp_entries == []
}

fn push_compatible_head(mut fixture TccHistoryFixture) string {
	compatible_head_sha := commit_bundle_state(fixture.source, 'compatible-v2',
		compatible_tcc_script('compatible-tcc-v2'), 'compatible-libgc-v2\n')
	run_checked('git -C ${os.quoted_path(fixture.source)} push --quiet ${os.quoted_path(fixture.remote)} HEAD:refs/heads/thirdparty-linux-amd64')
	return compatible_head_sha
}

fn push_compatible_head_without_local_include(mut fixture TccHistoryFixture) string {
	compatible_head_sha := commit_bundle_state(fixture.source, 'compatible-without-local-include',
		missing_local_include_tcc_script('compatible-without-local-include-tcc'),
		'compatible-without-local-include-libgc\n')
	run_checked('git -C ${os.quoted_path(fixture.source)} push --quiet ${os.quoted_path(fixture.remote)} HEAD:refs/heads/thirdparty-linux-amd64')
	return compatible_head_sha
}

fn test_linux_tcc_uses_newest_compatible_commit_and_returns_to_a_fixed_head() {
	if os.user_os() != 'linux' {
		return
	}
	mut fixture := new_tcc_history_fixture(true)
	defer {
		os.rmdir_all(fixture.root) or {}
	}

	fresh_result :=
		os.execute('export C_INCLUDE_PATH=/poison-c-include CPATH=/poison-cpath; ${fixture.fresh_cmd} 2>&1')
	assert fresh_result.exit_code == 0, fresh_result.output
	assert fresh_result.output.contains('is not host-compatible'), fresh_result.output
	assert fresh_result.output.contains('TCC search directories:'), fresh_result.output
	assert fresh_result.output.contains('include path environment is unavailable'), fresh_result.output
	assert fresh_result.output.contains('Using newest host-compatible TCC commit ${fixture.compatible_sha}'), fresh_result.output

	assert_historical_fallback(fixture)
	assert_fixture_bundle_is_vroot_cwd_sensitive(fixture)

	still_broken_result := os.execute('${fixture.latest_cmd} 2>&1')
	assert still_broken_result.exit_code == 0, still_broken_result.output
	assert still_broken_result.output.contains('is not host-compatible'), still_broken_result.output
	assert_historical_fallback(fixture)

	compatible_head_sha := push_compatible_head(mut fixture)
	fixed_result := os.execute('${fixture.latest_cmd} 2>&1')
	assert fixed_result.exit_code == 0, fixed_result.output
	assert git_current_branch(fixture.tcc_dir) == 'thirdparty-linux-amd64'
	assert run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space() == compatible_head_sha
	assert os.read_file(os.join_path(fixture.tcc_dir, 'lib', 'libgc.a'))! == 'compatible-libgc-v2\n'
	assert os.execute('${os.quoted_path(os.join_path(fixture.tcc_dir, 'tcc.exe'))} --version').output.trim_space() == 'compatible-tcc-v2'
	assert !os.exists(compatible_marker_dir(fixture))
	assert_clean_checkout(fixture.tcc_dir)
	assert os.ls(fixture.tmp_dir)! == []

	run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} config user.name "V Test"')
	run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} config user.email "v-test@example.invalid"')
	local_file := os.join_path(fixture.tcc_dir, 'user-local.txt')
	os.write_file(local_file, 'preserve this branch commit\n') or { panic(err) }
	run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} add user-local.txt')
	run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} commit --quiet -m user-local')
	local_sha :=
		run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space()
	local_result := os.execute('${fixture.latest_cmd} 2>&1')
	assert local_result.exit_code != 0, local_result.output
	assert local_result.output.contains('Refusing to overwrite local TCC commits'), local_result.output

	assert run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space() == local_sha
	assert os.read_file(local_file)! == 'preserve this branch commit\n'
	assert_clean_checkout(fixture.tcc_dir)
}

fn test_linux_tcc_accepts_compatible_bundle_without_usr_local_include() {
	if os.user_os() != 'linux' {
		return
	}
	mut fixture := new_tcc_history_fixture(false)
	defer {
		os.rmdir_all(fixture.root) or {}
	}

	compatible_head_sha := push_compatible_head_without_local_include(mut fixture)
	search_result :=
		os.execute('${os.quoted_path(os.join_path(fixture.source, 'tcc.exe'))} -print-search-dirs 2>&1')
	assert search_result.exit_code == 0, search_result.output
	assert !search_result.output.split_into_lines().contains('  /usr/local/include'), search_result.output

	fresh_result := os.execute('${fixture.fresh_cmd} 2>&1')
	assert fresh_result.exit_code == 0, fresh_result.output
	assert !fresh_result.output.contains('is not host-compatible'), fresh_result.output
	assert git_current_branch(fixture.tcc_dir) == 'thirdparty-linux-amd64'
	assert run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space() == compatible_head_sha
	assert os.read_file(os.join_path(fixture.tcc_dir, 'lib', 'libgc.a'))! == 'compatible-without-local-include-libgc\n'
	assert !os.exists(compatible_marker_dir(fixture))
	assert_clean_checkout(fixture.tcc_dir)
}

fn test_linux_tcc_preserves_git_command_options() {
	if os.user_os() != 'linux' {
		return
	}
	fixture := new_tcc_history_fixture(true)
	defer {
		os.rmdir_all(fixture.root) or {}
	}

	git_with_options := os.quoted_path('git -c protocol.file.allow=always')
	fresh_result := os.execute('${fixture.fresh_cmd} GIT=${git_with_options} 2>&1')
	assert fresh_result.exit_code == 0, fresh_result.output
	assert fresh_result.output.contains('Using newest host-compatible TCC commit ${fixture.compatible_sha}'), fresh_result.output

	assert_historical_fallback(fixture)

	latest_result := os.execute('${fixture.latest_cmd} GIT=${git_with_options} 2>&1')
	assert latest_result.exit_code == 0, latest_result.output
	assert latest_result.output.contains('Using newest host-compatible TCC commit ${fixture.compatible_sha}'), latest_result.output

	assert_historical_fallback(fixture)

	git_with_multiple_options :=
		os.quoted_path('  git   -c protocol.file.allow=always   -c advice.detachedHead=false  ')
	multiple_options_result :=
		os.execute('${fixture.latest_cmd} GIT=${git_with_multiple_options} 2>&1')
	assert multiple_options_result.exit_code == 0, multiple_options_result.output
	assert multiple_options_result.output.contains('Using newest host-compatible TCC commit ${fixture.compatible_sha}'), multiple_options_result.output

	assert_historical_fallback(fixture)

	wrapper_path := os.join_path(fixture.root, 'git-wrapper')
	wrapper_log := os.join_path(fixture.root, 'git-wrapper.log')
	write_executable(wrapper_path,
		'#!/bin/sh\nprintf "%s\\n" "\$*" >> ${os.quoted_path(wrapper_log)}\nexec git "\$@"\n')
	wrapper_result := os.execute('${fixture.latest_cmd} GIT=${os.quoted_path(wrapper_path)} 2>&1')
	assert wrapper_result.exit_code == 0, wrapper_result.output
	assert wrapper_result.output.contains('Using newest host-compatible TCC commit ${fixture.compatible_sha}'), wrapper_result.output
	assert os.read_file(wrapper_log)!.contains('ls-remote')

	assert_historical_fallback(fixture)
}

fn test_linux_tcc_latest_supports_legacy_git_branch_detection() {
	if os.user_os() != 'linux' {
		return
	}
	mut fixture := new_tcc_history_fixture(true)
	defer {
		os.rmdir_all(fixture.root) or {}
	}

	run_checked('${fixture.fresh_cmd} 2>&1')
	assert_historical_fallback(fixture)

	real_git := os.find_abs_path_of_executable('git') or { panic(err) }
	legacy_git := os.join_path(fixture.root, 'legacy-git')
	legacy_trace := os.join_path(fixture.root, 'legacy-git.trace')
	write_executable(legacy_git, '#!/bin/sh
set -eu
previous=
for argument in "\$@"; do
	if [ "\$previous" = branch ] && [ "\$argument" = --show-current ]; then
		echo "legacy git does not support branch --show-current" >&2
		exit 129
	fi
	previous="\$argument"
done
printf "%s\\n" "\$*" >> ${os.quoted_path(legacy_trace)}
exec ${os.quoted_path(real_git)} "\$@"
')
	legacy_git_arg := os.quoted_path(legacy_git)

	detached_result := os.execute('${fixture.latest_cmd} GIT=${legacy_git_arg} 2>&1')
	assert detached_result.exit_code == 0, detached_result.output
	assert_historical_fallback(fixture)

	compatible_head_sha := push_compatible_head(mut fixture)
	repair_result := os.execute('${fixture.latest_cmd} GIT=${legacy_git_arg} 2>&1')
	assert repair_result.exit_code == 0, repair_result.output
	assert git_current_branch(fixture.tcc_dir) == 'thirdparty-linux-amd64'
	assert run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space() == compatible_head_sha

	branch_result := os.execute('${fixture.latest_cmd} GIT=${legacy_git_arg} 2>&1')
	assert branch_result.exit_code == 0, branch_result.output
	assert git_current_branch(fixture.tcc_dir) == 'thirdparty-linux-amd64'
	assert_clean_checkout(fixture.tcc_dir)

	trace := os.read_file(legacy_trace) or { panic(err) }
	assert !trace.contains('branch --show-current'), trace
	assert trace.count('symbolic-ref --quiet --short HEAD') >= 3, trace
}

fn test_linux_tcc_does_not_evaluate_git_command_during_make_parsing() {
	if os.user_os() != 'linux' {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_make_git_parse_${rand.ulid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	shell_sentinel := os.join_path(root, 'shell-sentinel')
	make_sentinel := os.join_path(root, 'make-sentinel')
	git_specs := [
		'git; touch ${shell_sentinel} #',
		'git $(shell touch ${make_sentinel})',
	]
	for git_spec in git_specs {
		result :=
			os.execute('cd ${os.quoted_path(root)} && make --no-print-directory -n -f ${os.quoted_path(makefile_path)} fresh_tcc VROOT=${os.quoted_path(root)} TCCOS=linux TCCARCH=amd64 GIT=${os.quoted_path(git_spec)} 2>&1')
		assert result.exit_code == 0, result.output
		assert !os.exists(shell_sentinel)
		assert !os.exists(make_sentinel)
	}
}

fn test_linux_tcc_git_detection_does_not_depend_on_shell_export() {
	if os.user_os() != 'linux' {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_make_43_git_${rand.ulid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	make_43_shell := os.join_path(root, 'make-4.3-shell')
	write_executable(make_43_shell, '#!/bin/sh\nunset GIT GIT_PROGRAM\nexec /bin/sh "\$@"\n')
	missing_git := 'v-missing-git-${rand.ulid()}'
	result :=
		os.execute('make --no-print-directory -pn -f ${os.quoted_path(makefile_path)} latest_tcc VROOT=${os.quoted_path(root)} TCCOS=linux TCCARCH=amd64 GIT=${os.quoted_path(missing_git)} SHELL=${os.quoted_path(make_43_shell)} 2>&1')
	assert result.exit_code == 0, result.output
	assert result.output.contains("select_linux_tcc.sh' latest"), result.output

	makefile_contents := os.read_file(makefile_path)!
	linux_git_block :=
		makefile_contents.all_after('ifeq ($(TCCOS),linux)\n').all_before('\nelse\nexport GIT\nHAS_GIT :=')
	assert linux_git_block.contains('export GIT'), linux_git_block
	assert !linux_git_block.contains('HAS_GIT := $(shell'), linux_git_block
	assert !linux_git_block.contains('command -v $(GIT)'), linux_git_block
	assert !linux_git_block.contains('$$GIT_PROGRAM'), linux_git_block
	assert !linux_git_block.contains('GIT_PROGRAM :='), linux_git_block
}

fn test_linux_missing_or_unsafe_git_preserves_existing_vc() {
	if os.user_os() != 'linux' {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_make_vc_git_${rand.ulid()}')
	vc_dir := os.join_path(root, 'vc')
	os.mkdir_all(vc_dir) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.symlink(makefile_path, os.join_path(root, 'GNUmakefile')) or { panic(err) }
	vc_file := os.join_path(vc_dir, 'v.c')
	os.write_file(vc_file, '/* preserve manual vc */\n') or { panic(err) }
	sentinel := os.join_path(root, 'sentinel')
	git_specs := [
		'v-missing-git-${rand.ulid()}',
		'git; touch ${sentinel} #',
		'git $(touch ${sentinel})',
	]
	for git_spec in git_specs {
		result :=
			os.execute('cd ${os.quoted_path(root)} && make --no-print-directory -f ${os.quoted_path(makefile_path)} latest_vc VROOT=${os.quoted_path(root)} GIT_ARGV_RUNNER=${os.quoted_path(git_argv_path)} TCCOS=linux GIT=${os.quoted_path(git_spec)} 2>&1')
		assert result.exit_code == 0, '${git_spec}:\n${result.output}'
		assert result.output.contains('using existing ./vc/v.c'), result.output
		assert os.read_file(vc_file)! == '/* preserve manual vc */\n'
		assert !os.exists(sentinel)

		fresh_result :=
			os.execute('cd ${os.quoted_path(root)} && make --no-print-directory -f ${os.quoted_path(makefile_path)} fresh_vc VROOT=${os.quoted_path(root)} GIT_ARGV_RUNNER=${os.quoted_path(git_argv_path)} TCCOS=linux GIT=${os.quoted_path(git_spec)} 2>&1')
		assert fresh_result.exit_code != 0, '${git_spec}:\n${fresh_result.output}'
		assert os.read_file(vc_file)! == '/* preserve manual vc */\n'
		assert !os.exists(sentinel)
	}

	remote := os.join_path(root, 'vc.git')
	source := os.join_path(root, 'vc-source')
	run_checked('git init --quiet --bare ${os.quoted_path(remote)}')
	configure_source_repo(source)
	os.write_file(os.join_path(source, 'v.c'), '/* cloned vc */\n') or { panic(err) }
	run_checked('git -C ${os.quoted_path(source)} add v.c')
	run_checked('git -C ${os.quoted_path(source)} commit --quiet -m vc')
	run_checked('git -C ${os.quoted_path(source)} push --quiet ${os.quoted_path(remote)} HEAD:refs/heads/master')
	run_checked('git -C ${os.quoted_path(remote)} symbolic-ref HEAD refs/heads/master')

	git_path := run_checked('command -v git').trim_space()
	wrapper_path := os.join_path(root, 'git-wrapper')
	wrapper_log := os.join_path(root, 'git-wrapper.log')
	write_executable(wrapper_path,
		'#!/bin/sh\nprintf "%s\\n" "\$*" >> ${os.quoted_path(wrapper_log)}\nexec ${os.quoted_path(git_path)} "\$@"\n')
	valid_git_specs := [
		git_path,
		'git -c protocol.file.allow=always',
		wrapper_path,
	]
	os.rmdir_all(vc_dir) or { panic(err) }
	bootstrap_result :=
		os.execute('cd ${os.quoted_path(root)} && make --no-print-directory -f ${os.quoted_path(makefile_path)} latest_vc VROOT=${os.quoted_path(root)} GIT_ARGV_RUNNER=${os.quoted_path(git_argv_path)} VCREPO=${os.quoted_path(remote)} TCCOS=linux GIT=${os.quoted_path(git_path)} 2>&1')
	assert bootstrap_result.exit_code == 0, bootstrap_result.output
	assert os.read_file(vc_file)! == '/* cloned vc */\n'

	for git_spec in valid_git_specs {
		result :=
			os.execute('cd ${os.quoted_path(root)} && make --no-print-directory -f ${os.quoted_path(makefile_path)} fresh_vc VROOT=${os.quoted_path(root)} GIT_ARGV_RUNNER=${os.quoted_path(git_argv_path)} VCREPO=${os.quoted_path(remote)} TCCOS=linux GIT=${os.quoted_path(git_spec)} 2>&1')
		assert result.exit_code == 0, '${git_spec}:\n${result.output}'
		assert os.read_file(vc_file)! == '/* cloned vc */\n'
		latest_result :=
			os.execute('cd ${os.quoted_path(root)} && make --no-print-directory -f ${os.quoted_path(makefile_path)} latest_vc VROOT=${os.quoted_path(root)} GIT_ARGV_RUNNER=${os.quoted_path(git_argv_path)} VCREPO=${os.quoted_path(remote)} TCCOS=linux GIT=${os.quoted_path(git_spec)} 2>&1')
		assert latest_result.exit_code == 0, '${git_spec}:\n${latest_result.output}'
		assert os.read_file(vc_file)! == '/* cloned vc */\n'
	}
	assert os.read_file(wrapper_log)!.contains('clone')
}

fn test_linux_tcc_rejects_unsafe_git_command_data() {
	if os.user_os() != 'linux' {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_selector_git_data_${rand.ulid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'thirdparty')) or { panic(err) }
	sentinel := os.join_path(root, 'sentinel')
	unsafe_git_specs := [
		'git; touch ${sentinel}',
		'git && touch ${sentinel}',
		'git $(touch ${sentinel})',
		'git `touch ${sentinel}`',
		'git > ${sentinel}',
		'git *',
		'git "quoted path"',
		'git\\ wrapper',
		'git\n-c protocol.file.allow=always',
	]
	selector_args := '${os.quoted_path(selector_path)} fresh ${os.quoted_path(os.join_path(root,
		'thirdparty', 'tcc'))} unused amd64 ${os.quoted_path(root)}'
	for git_spec in unsafe_git_specs {
		result := os.execute('GIT=${os.quoted_path(git_spec)} bash ${selector_args} 2>&1')
		assert result.exit_code == 2, '${git_spec}:\n${result.output}'
		assert result.output.contains('the Git command contains unsupported characters'), result.output
		assert !os.exists(sentinel)
	}

	missing_git := 'v-missing-git-${rand.ulid()}'
	missing_result := os.execute('GIT=${os.quoted_path(missing_git)} bash ${selector_args} 2>&1')
	assert missing_result.exit_code == 2, missing_result.output
	assert missing_result.output.contains('the Git executable was not found: ${missing_git}'), missing_result.output
}

fn test_linux_tcc_missing_git_preserves_latest_and_fails_fresh() {
	if os.user_os() != 'linux' {
		return
	}
	fixture := new_tcc_history_fixture(true)
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	run_checked('${fixture.fresh_cmd} 2>&1')
	assert_historical_fallback(fixture)
	head_before :=
		run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space()
	metadata_path := os.join_path(compatible_marker_dir(fixture), 'metadata')
	metadata_before := os.read_file(metadata_path)!
	missing_git := 'v-missing-git-${rand.ulid()}'

	latest_result := os.execute('${fixture.latest_cmd} GIT=${os.quoted_path(missing_git)} 2>&1')
	assert latest_result.exit_code == 0, latest_result.output
	assert latest_result.output.contains('the Git executable was not found: ${missing_git}; skipping the Linux TCC refresh.'), latest_result.output
	assert run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space() == head_before
	assert os.read_file(metadata_path)! == metadata_before
	assert_historical_fallback(fixture)

	explicit_latest_result :=
		os.execute('${fixture.latest_cmd} GIT=${os.quoted_path(missing_git)} VFLAGS="-cc tcc" 2>&1')
	assert explicit_latest_result.exit_code == 0, explicit_latest_result.output
	assert explicit_latest_result.output.contains('preserving the existing host-compatible TCC bundle'), explicit_latest_result.output
	assert_historical_fallback(fixture)

	fresh_result := os.execute('${fixture.fresh_cmd} GIT=${os.quoted_path(missing_git)} 2>&1')
	assert fresh_result.exit_code != 0, fresh_result.output
	assert fresh_result.output.contains('the Git executable was not found: ${missing_git}'), fresh_result.output
	assert !os.exists(fixture.tcc_dir)

	empty_latest_result :=
		os.execute('${fixture.latest_cmd} GIT=${os.quoted_path(missing_git)} 2>&1')
	assert empty_latest_result.exit_code == 0, empty_latest_result.output
	assert empty_latest_result.output.contains('skipping the Linux TCC refresh'), empty_latest_result.output
	assert !os.exists(fixture.tcc_dir)
	assert os.ls(fixture.tmp_dir)! == []

	explicit_empty_result :=
		os.execute('${fixture.latest_cmd} GIT=${os.quoted_path(missing_git)} VFLAGS="-cc=tcc" 2>&1')
	assert explicit_empty_result.exit_code != 0, explicit_empty_result.output
	assert explicit_empty_result.output.contains("existing TCC bundle failed its host-compatibility probe; explicit '-cc tcc' cannot continue"), explicit_empty_result.output
	assert !os.exists(fixture.tcc_dir)

	os.mkdir_all(os.join_path(fixture.tcc_dir, 'lib')) or { panic(err) }
	os.write_file(os.join_path(fixture.tcc_dir, 'lib', 'libgc.a'), 'broken-libgc\n') or {
		panic(err)
	}
	os.symlink('/bin/false', os.join_path(fixture.tcc_dir, 'tcc.exe')) or { panic(err) }
	broken_tcc_result :=
		os.execute('${fixture.latest_cmd} GIT=${os.quoted_path(missing_git)} VFLAGS="-cc tcc" 2>&1')
	assert broken_tcc_result.exit_code != 0, broken_tcc_result.output
	assert broken_tcc_result.output.contains('existing TCC bundle failed its host-compatibility probe'), broken_tcc_result.output
	assert os.is_link(os.join_path(fixture.tcc_dir, 'tcc.exe'))
	assert os.ls(fixture.tmp_dir)! == []

	source_cmd := fixture.latest_cmd.replace_once(' latest_tcc ', ' latest_tcc_source ')
	missing_source_result := os.execute('${source_cmd} GIT=${os.quoted_path(missing_git)} 2>&1')
	assert missing_source_result.exit_code != 0, missing_source_result.output
	assert missing_source_result.output.contains('the Git executable was not found: ${missing_git}'), missing_source_result.output
	assert os.is_link(os.join_path(fixture.tcc_dir, 'tcc.exe'))
	assert os.read_file(os.join_path(fixture.tcc_dir, 'lib', 'libgc.a'))! == 'broken-libgc\n'

	source_sentinel := os.join_path(fixture.root, 'source-sentinel')
	hostile_source_git := 'git; touch ${source_sentinel} #'
	hostile_source_result :=
		os.execute('${source_cmd} GIT=${os.quoted_path(hostile_source_git)} 2>&1')
	assert hostile_source_result.exit_code != 0, hostile_source_result.output
	assert hostile_source_result.output.contains('the Git command contains unsupported characters'), hostile_source_result.output
	assert !os.exists(source_sentinel)
	assert os.is_link(os.join_path(fixture.tcc_dir, 'tcc.exe'))
	assert os.read_file(os.join_path(fixture.tcc_dir, 'lib', 'libgc.a'))! == 'broken-libgc\n'

	source_result := os.execute('${source_cmd} 2>&1')
	assert source_result.exit_code != 0, source_result.output
	assert source_result.output.contains('No upstream TinyCC build script is available'), source_result.output
	assert_historical_fallback(fixture)
}

fn test_linux_tcc_explicit_request_does_not_hide_missing_compatible_history() {
	if os.user_os() != 'linux' {
		return
	}
	fixture := new_tcc_history_fixture(false)
	defer {
		os.rmdir_all(fixture.root) or {}
	}

	result := os.execute('${fixture.fresh_cmd} VFLAGS="-cc tcc" 2>&1')
	assert result.exit_code != 0, result.output
	assert result.output.contains('No host-compatible TCC commit was found'), result.output
	assert result.output.contains("explicit '-cc tcc' cannot continue"), result.output
	assert !result.output.contains('using the system compiler'), result.output
	assert os.ls(fixture.tmp_dir)! == []
}

fn test_linux_tcc_without_explicit_request_uses_system_fallback() {
	if os.user_os() != 'linux' {
		return
	}
	mut fixture := new_tcc_history_fixture(false)
	defer {
		os.rmdir_all(fixture.root) or {}
	}

	result := os.execute('${fixture.fresh_cmd} 2>&1')
	assert result.exit_code == 0, result.output
	assert result.output.contains('using the system compiler'), result.output
	assert git_current_branch(fixture.tcc_dir) == 'thirdparty-unknown-unknown'
	metadata := os.read_file(os.join_path(compatible_marker_dir(fixture), 'metadata'))!
	assert metadata == 'tccos=linux\ntccarch=amd64\nabi=glibc\nbranch=thirdparty-linux-amd64\nremote_head_sha=${fixture.incompatible_sha}\nmode=system\n', metadata

	assert_clean_checkout(fixture.tcc_dir)
	assert os.ls(fixture.tmp_dir)! == []

	compatible_head_sha := push_compatible_head(mut fixture)
	refresh_result := os.execute('${fixture.latest_cmd} 2>&1')
	assert refresh_result.exit_code == 0, refresh_result.output
	assert git_current_branch(fixture.tcc_dir) == 'thirdparty-linux-amd64'
	assert run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space() == compatible_head_sha
	assert !os.exists(compatible_marker_dir(fixture))
	assert_clean_checkout(fixture.tcc_dir)
}

fn test_linux_tcc_retries_an_initially_missing_native_branch() {
	if os.user_os() != 'linux' {
		return
	}
	mut fixture := new_tcc_history_fixture(true)
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	run_checked('git -C ${os.quoted_path(fixture.source)} push --quiet ${os.quoted_path(fixture.remote)} :refs/heads/thirdparty-linux-amd64')

	fresh_result := os.execute('${fixture.fresh_cmd} 2>&1')
	assert fresh_result.exit_code == 0, fresh_result.output
	assert fresh_result.output.contains('using the system compiler'), fresh_result.output
	metadata_path := os.join_path(compatible_marker_dir(fixture), 'metadata')
	assert os.read_file(metadata_path)! == 'tccos=linux\ntccarch=amd64\nabi=glibc\nbranch=thirdparty-linux-amd64\nremote_head_sha=unavailable\nmode=system\n'

	still_missing_result := os.execute('${fixture.latest_cmd} 2>&1')
	assert still_missing_result.exit_code == 0, still_missing_result.output
	assert still_missing_result.output.contains('continuing with the system compiler'), still_missing_result.output

	assert os.read_file(metadata_path)!.contains('remote_head_sha=unavailable\n')
	assert_clean_checkout(fixture.tcc_dir)

	compatible_head_sha := push_compatible_head(mut fixture)
	repaired_result := os.execute('${fixture.latest_cmd} 2>&1')
	assert repaired_result.exit_code == 0, repaired_result.output
	assert git_current_branch(fixture.tcc_dir) == 'thirdparty-linux-amd64'
	assert run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space() == compatible_head_sha
	assert !os.exists(compatible_marker_dir(fixture))
	assert_clean_checkout(fixture.tcc_dir)
}

fn test_latest_tcc_refuses_local_commits_in_system_fallback() {
	if os.user_os() != 'linux' {
		return
	}
	fixture := new_tcc_history_fixture(false)
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	run_checked('${fixture.fresh_cmd} 2>&1')
	run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} config user.name "V Test"')
	run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} config user.email "v-test@example.invalid"')
	local_file := os.join_path(fixture.tcc_dir, 'user-local.txt')
	os.write_file(local_file, 'preserve this commit\n') or { panic(err) }
	run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} add user-local.txt')
	run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} commit --quiet -m user-local')
	local_sha :=
		run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space()

	result := os.execute('${fixture.latest_cmd} 2>&1')
	assert result.exit_code != 0, result.output
	assert result.output.contains('while it contains local commits'), result.output
	assert run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space() == local_sha
	assert os.read_file(local_file)! == 'preserve this commit\n'
	assert_clean_checkout(fixture.tcc_dir)
}

fn test_latest_tcc_refuses_dirty_or_mismatched_detached_checkout() {
	if os.user_os() != 'linux' {
		return
	}
	fixture := new_tcc_history_fixture(true)
	defer {
		os.rmdir_all(fixture.root) or {}
	}
	run_checked('${fixture.fresh_cmd} 2>&1')
	assert_historical_fallback(fixture)

	local_file := os.join_path(fixture.tcc_dir, 'user-local.txt')
	os.write_file(local_file, 'preserve me\n') or { panic(err) }
	dirty_result := os.execute('${fixture.latest_cmd} 2>&1')
	assert dirty_result.exit_code != 0, dirty_result.output
	assert dirty_result.output.contains('Refusing to refresh a dirty TCC checkout'), dirty_result.output
	assert os.read_file(local_file)! == 'preserve me\n'
	os.rm(local_file)!

	mismatch_result := os.execute('${fixture.latest_cmd} TCCARCH=arm64 2>&1')
	assert mismatch_result.exit_code != 0, mismatch_result.output
	assert mismatch_result.output.contains('Refusing to refresh detached TCC without an exact'), mismatch_result.output

	assert_historical_fallback(fixture)
	run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} checkout --quiet -b user-preserved-branch')
	wrong_branch_result := os.execute('${fixture.latest_cmd} 2>&1')
	assert wrong_branch_result.exit_code != 0, wrong_branch_result.output
	assert wrong_branch_result.output.contains('Refusing to refresh TCC branch user-preserved-branch'), wrong_branch_result.output

	assert git_current_branch(fixture.tcc_dir) == 'user-preserved-branch'
	assert run_checked('git -C ${os.quoted_path(fixture.tcc_dir)} rev-parse HEAD').trim_space() == fixture.compatible_sha
}

fn test_linuxmusl_native_bundle_stays_on_its_own_clean_branch() {
	if os.user_os() != 'linux' {
		return
	}
	fixture := new_tcc_history_fixture(true)
	defer {
		os.rmdir_all(fixture.root) or {}
	}

	result := os.execute('${fixture.fresh_cmd} TCCOS=linuxmusl TCCARCH=amd64 2>&1')
	assert result.exit_code == 0, result.output
	assert git_current_branch(fixture.tcc_dir) == 'thirdparty-linuxmusl-amd64'
	assert os.read_file(os.join_path(fixture.tcc_dir, 'lib', 'libgc.a'))! == 'musl-libgc\n'
	assert !os.exists(compatible_marker_dir(fixture))
	assert_clean_checkout(fixture.tcc_dir)
	assert os.ls(fixture.tmp_dir)! == []
}

fn execute_tcc_gc_probe_without_vflags(command string) os.Result {
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	result := os.execute(command)
	if vflags := old_vflags {
		os.setenv('VFLAGS', vflags, true)
	} else {
		os.unsetenv('VFLAGS')
	}
	return result
}

fn test_linux_glibc_bundled_tcc_links_and_runs_boehm_gc() {
	$if !linux {
		return
	}
	$if !amd64 {
		return
	}
	if os.execute('ldd --version 2>&1').output.to_lower().contains('musl') {
		return
	}
	tcc_dir := os.join_path(@VEXEROOT, 'thirdparty', 'tcc')
	tcc_path := os.join_path(tcc_dir, 'tcc.exe')
	if !os.is_executable(tcc_path)
		|| os.execute('${os.quoted_path(tcc_path)} --version').exit_code != 0 {
		return
	}
	// The hermetic repositories prove history selection with executable fixtures.
	// A native historical-fallback lane sets this gate before running the real
	// compiler+libgc consumer below.
	if os.getenv('V_CI_TCC_HISTORICAL_FALLBACK_REAL') == '1' {
		assert git_current_branch(tcc_dir) == ''
		metadata_path := os.join_path(tcc_dir, '.git', 'vlang-compatible-tcc', 'metadata')
		metadata := os.read_file(metadata_path)!
		head_sha := run_checked('git -C ${os.quoted_path(tcc_dir)} rev-parse HEAD').trim_space()
		assert metadata.contains('abi=glibc\n'), metadata
		assert metadata.contains('branch=thirdparty-linux-amd64\n'), metadata
		assert metadata.contains('compatible_sha=${head_sha}\n'), metadata
	}
	probe_dir := os.join_path(os.vtmp_dir(), 'v_make_tcc_gc_probe_${rand.ulid()}')
	os.mkdir_all(probe_dir) or { panic(err) }
	defer {
		os.rmdir_all(probe_dir) or {}
	}
	source_path := os.join_path(probe_dir, 'main.v')
	executable_path := os.join_path(probe_dir, 'probe')
	os.write_file(source_path,
		"fn main() {\n\tmut values := []string{}\n\tvalues << 'historical-tcc-gc-ok'\n\tprintln(values[0])\n}\n") or {
		panic(err)
	}
	build_command := '${os.quoted_path(@VEXE)} -showcc -cc ${os.quoted_path(tcc_path)} -gc boehm -no-retry-compilation -o ${os.quoted_path(executable_path)} ${os.quoted_path(source_path)}'
	build_result := execute_tcc_gc_probe_without_vflags(build_command)
	assert build_result.exit_code == 0, build_result.output
	assert build_result.output.contains('thirdparty/tcc/lib/libgc.a'), build_result.output
	assert !build_result.output.contains('sigsetjmp'), build_result.output
	run_result := os.execute(os.quoted_path(executable_path))
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'historical-tcc-gc-ok'
}
