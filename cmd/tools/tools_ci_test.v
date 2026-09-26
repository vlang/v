// vtest build: !windows
// Run with ./v cmd/tools/tools_ci_test.v on a POSIX host.
// Execute the workflow's shell blocks, replacing external commands and ulimit
// with isolated fixtures. No BSD VM, package installation, or network is used.
import os
import time

const tools_ci_probe = 'label=$1
shift
for arg in "$@"; do
    label="$label $arg"
done
printf \'%s\\n\' "$label" >> "$CI_TEST_LOG"
if [ "$label" = "$CI_TEST_FAIL" ]; then
    exit "$CI_TEST_CODE"
fi
exit 0
'

struct ToolsCiJob {
	name      string
	compilers []string
}

fn tools_ci_jobs() []ToolsCiJob {
	return [
		ToolsCiJob{'tools-freebsd', ['tcc', 'gcc', 'clang']},
		ToolsCiJob{'tools-openbsd', ['tcc', 'clang']},
	]
}

fn tools_ci_workflow() !string {
	return os.read_file(os.join_path(@VEXEROOT, '.github', 'workflows', 'tools_ci.yml'))
}

// Read only the known job/literal-block structure, not arbitrary YAML syntax.
// Reject missing or ambiguous matches instead of silently testing another step.
fn tools_ci_job_text(workflow string, name string) !string {
	lines := workflow.split_into_lines()
	label := '  ${name}:'
	mut starts := []int{}
	for i, line in lines {
		if line == label {
			starts << i + 1
		}
	}
	if starts.len != 1 {
		return error('expected exactly one ${name} job, found ${starts.len}')
	}
	start := starts[0]
	mut end := start
	for end < lines.len {
		line := lines[end]
		if line.trim_space() != '' && !line.starts_with('    ') {
			break
		}
		end++
	}
	return lines[start..end].join('\n') + '\n'
}

fn tools_ci_bsd_script(job string) !string {
	lines := job.split_into_lines()
	mut starts := []int{}
	for i, line in lines {
		if line == '        shell: cpa.sh {0}' {
			starts << i + 1
		}
	}
	if starts.len != 1 {
		return error('expected exactly one cpa.sh literal block, found ${starts.len}')
	}
	start := starts[0]
	if start >= lines.len || lines[start] != '        run: |' {
		return error('expected a literal run block immediately after cpa.sh')
	}
	mut body := []string{}
	for line in lines[start + 1..] {
		if line == '' {
			body << ''
		} else if line.starts_with('          ') {
			body << line[10..]
		} else {
			break
		}
	}
	script := body.join('\n').trim_right('\n') + '\n'
	if script.trim_space() == '' || script.contains(r'${{') {
		return error('expected a nonempty shell block without Actions expressions')
	}
	return script
}

fn tools_ci_compilers(job string) ![]string {
	mut matrices := []string{}
	for line in job.split_into_lines() {
		if line.starts_with('        cc: [') && line.ends_with(']') {
			matrices << line.all_after('[').all_before_last(']')
		}
	}
	if matrices.len != 1 {
		return error('expected exactly one compiler matrix, found ${matrices.len}')
	}
	return matrices[0].split(',').map(it.trim_space())
}

fn tools_ci_expected_commands(job string, compiler string) []string {
	mut commands := []string{}
	if job == 'tools-freebsd' {
		commands << 'pkg install -y git sqlite3 gmake boehm-gc-threaded libiconv'
		if compiler == 'gcc' {
			commands << 'pkg install -y gcc'
		}
		commands << 'hostname -s freebsd-ci'
	} else {
		commands << ['pkg_add git sqlite3 gmake boehm-gc libiconv openssl',
			'hostname -s openbsd-ci']
	}
	commands << ['uname -a', 'git config --global --add safe.directory .']
	if job == 'tools-openbsd' {
		commands << 'ulimit -d 4194304'
	}
	commands << ['gmake', 'v -showcc -o v cmd/v', 'v symlink', 'v doctor',
		'v fmt -verify cmd/', 'v -silent -N -W -check build-tools', 'v -silent test-self cmd']
	if compiler != 'tcc' {
		commands << 'v -silent -W -cstrict test-self cmd'
	}
	return commands
}

fn tools_ci_write_executable(path string, source string) ! {
	os.write_file(path, source)!
	os.chmod(path, 0o755)!
}

fn tools_ci_create_fixtures(root string, sh string) ! {
	bin_dir := os.join_path(root, 'bin')
	os.mkdir_all(bin_dir)!
	shebang := '#!${sh}\n'
	tools_ci_write_executable(os.join_path(root, 'probe'), shebang + tools_ci_probe)!
	for command in ['pkg', 'pkg_add', 'hostname', 'uname', 'git', 'gmake', 'v'] {
		path := os.join_path(if command == 'v' { root } else { bin_dir }, command)
		tools_ci_write_executable(path, shebang + 'exec "$CI_TEST_PROBE" ${command} "$@"\n')!
	}
	tools_ci_write_executable(os.join_path(bin_dir, 'sudo'), shebang + 'exec "$@"\n')!
}

fn tools_ci_execute(root string, script string, shell string, errexit bool, compiler string, failure string, code int) !(os.Result, []string) {
	log := os.join_path(root, 'commands.log')
	output_path := os.join_path(root, 'output.log')
	os.write_file(log, '')!
	os.write_file(output_path, '')!
	// File-backed output cannot block the child on a full pipe. Replacing the
	// ulimit builtin avoids changing real resource limits in the shell fixture.
	prelude := 'exec > "$CI_TEST_OUTPUT" 2>&1\nulimit() { "$CI_TEST_PROBE" ulimit "$@"; }\n'
	mut args := []string{}
	if errexit {
		args << '-e'
	}
	args << ['-c', prelude + script]
	mut process := os.new_process(shell)
	process.set_args(args)
	process.set_work_folder(root)
	// Deliberately do not inherit the environment: PATH contains only fixtures.
	process.set_environment({
		'PATH':           os.join_path(root, 'bin')
		'LC_ALL':         'C'
		'VFLAGS':         '-cc ${compiler}'
		'CI_TEST_PROBE':  os.join_path(root, 'probe')
		'CI_TEST_LOG':    log
		'CI_TEST_OUTPUT': output_path
		'CI_TEST_FAIL':   failure
		'CI_TEST_CODE':   code.str()
	})
	process.use_pgroup = true
	process.run()
	defer {
		process.close()
	}
	sw := time.new_stopwatch()
	for process.is_alive() {
		if sw.elapsed() >= 5 * time.second {
			process.signal_pgkill()
			process.signal_kill()
			// signal_kill marks the child aborted before wait has reaped it.
			if process.status == .aborted {
				process.status = .running
			}
			process.wait()
			return error('shell fixture timed out: ${shell}, ${compiler}, ${failure}')
		}
		time.sleep(time.millisecond)
	}
	process.wait()
	if process.err != '' {
		return error(process.err)
	}
	output := os.read_file(output_path)!
	trace := os.read_lines(log)!
	return os.Result{process.code, output}, trace
}

fn test_tools_ci_extracts_the_literal_script_and_rejects_ambiguous_structure() {
	workflow := 'jobs:\n  example:\n    steps:\n      - name: probe\n        shell: cpa.sh {0}\n        run: |\n          echo first\n\n          echo second\n\n  other:\n    runs-on: unused\n'
	job := tools_ci_job_text(workflow, 'example')!
	assert tools_ci_bsd_script(job)! == 'echo first\n\necho second\n'
	for changed in [workflow.replace('  example:', '  removed:'), workflow + workflow] {
		if _ := tools_ci_job_text(changed, 'example') {
			assert false, 'accepted a missing or duplicate job'
		}
	}
	for changed in [job + job, job.replace('        run: |', '        run: >'),
		job.replace('echo first', r'echo ${{ matrix.cc }}')] {
		if _ := tools_ci_bsd_script(changed) {
			assert false, 'accepted an ambiguous or unsupported shell block'
		}
	}
}

fn test_tools_ci_bsd_failure_propagation() {
	workflow := tools_ci_workflow()!
	sh := os.find_abs_path_of_executable('sh')!
	mut shells := [sh]
	if bash := os.find_abs_path_of_executable('bash') {
		shells << bash
	}
	root := os.join_path(os.vtmp_dir(), 'v tools ci ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	tools_ci_create_fixtures(root, sh)!
	mut successes := 0
	mut failures := 0
	for job in tools_ci_jobs() {
		body := tools_ci_job_text(workflow, job.name)!
		assert tools_ci_compilers(body)! == job.compilers, job.name
		script := tools_ci_bsd_script(body)!
		for compiler in job.compilers {
			expected := tools_ci_expected_commands(job.name, compiler)
			for shell in shells {
				for errexit in [false, true] {
					label := '${job.name}, ${compiler}, ${shell}, errexit=${errexit}'
					result, trace := tools_ci_execute(root, script, shell, errexit, compiler, '', 0)!
					assert result.exit_code == 0, '${label}: ${result.output}'
					assert trace == expected, '${label}: ${trace}'
					successes++
					for index, failure in expected {
						// Distinct non-1 codes must survive unchanged.
						code := 41 + index
						failed, stopped_trace := tools_ci_execute(root, script, shell, errexit, compiler,
							failure, code)!
						assert failed.exit_code == code, '${label}, ${failure}: ${failed.output}'
						assert stopped_trace == expected[..index + 1], '${label}, ${failure}: ${stopped_trace}'
						failures++
					}
				}
			}
		}
	}
	assert successes == 10 * shells.len
	assert failures == 122 * shells.len
	println('BSD shell contracts: ${successes} successful workloads, ${failures} injected failures')
}
