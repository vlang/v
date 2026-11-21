module testing

import os
import time
import runtime
import benchmark
import sync.pool
import v.cmdexec

// Most of the time of a module's internal test (a `_test.v` file that declares the
// module it tests) is the compilation of the whole module, and a small `main` test
// pays the fixed cost of a whole program (the runtime, the C compilation, the link).
// The runner builds up to `VTEST_BATCH_SIZE` such files of one directory and module
// into one binary, and runs it once. A batch that fails to compile or to run is run again file by file,
// so a failure is reported exactly as without batching. `VTEST_NO_BATCH=1` turns
// batching off.
const test_batch_size_env = 'VTEST_BATCH_SIZE'
const test_batch_disable_env = 'VTEST_NO_BATCH'
const default_test_batch_size = 16
// A batch compiles several programs as one, and a combination can need far more
// memory than any of its files alone (several empty interfaces in one program grow
// cgen's memory steeply). It gets a limit of its own, so such a batch fails early
// and its files run one by one under the usual limit.
const test_batch_memory_limit_env = 'VTEST_BATCH_MEMORY_LIMIT'
const default_test_batch_memory_limit_mib = 4096

// A file that uses one of these is run on its own: test directives configure one
// file, suite hooks run once per binary, and the rest change or depend on state of
// the whole process, which the other tests of a batch would share. A `fn main` of
// a module's internal test is an ordinary function of the module; one in two
// files collides by name.
const test_batch_excluded_markers = ['// vtest', '// v test', 'fn testsuite_begin', 'fn testsuite_end',
	'os.executable(', 'os.setenv(', 'os.unsetenv(', 'os.chdir(', 'os.args', 'arguments()', 'exit(']

// A `main` test is a program of its own. These change how all of a program is
// compiled or linked (C flags and headers, exported or global symbols), so a file
// with one of them is not built together with other programs.
const test_batch_main_excluded_markers = ['#flag', '#include', '#pkgconfig', '@[export', '__global',
	'@[noinit]', '@[console]', '@[windows_stdcall]']

struct TestBatchCandidate {
	key   string
	names []string
}

struct TestBatch {
	key string
mut:
	files []string
	names map[string]bool
}

fn test_batch_memory_limit_mib() int {
	value := os.getenv(test_batch_memory_limit_env).int()
	return if value > 0 { value } else { default_test_batch_memory_limit_mib }
}

fn test_batch_size() int {
	value := os.getenv(test_batch_size_env)
	if value == '' {
		return default_test_batch_size
	}
	return value.int()
}

// can_batch_tests reports whether this session compiles and runs plain test files,
// the only mode that batches them.
fn (ts &TestSession) can_batch_tests() bool {
	if os.getenv(test_batch_disable_env) !in ['', '0'] {
		return false
	}
	if !ts.will_compile || ts.exec_mode != .compile_and_run || ts.show_stats || ts.build_tools
		|| ts.resume_dir != '' {
		return false
	}
	vargs := os.split_args(ts.vargs) or { return false }
	for arg in vargs {
		if arg in ['fmt', 'vet', '-b', '-backend', '-stats', '-o', '-output'] {
			return false
		}
	}
	return test_batch_size() > 1
}

// test_units returns what the pool of test runners works on: each element is
// either a single file, or the files of one batch joined with newlines. A batch
// takes the place of its first file in the order of `files`.
fn (ts &TestSession) test_units(files []string) []string {
	if !ts.can_batch_tests() {
		return files.clone()
	}
	max_size := test_batch_size()
	mut skipped := map[string]bool{}
	for file in ts.skip_files {
		skipped[file] = true
	}
	mut batches := []TestBatch{}
	mut open_batches := map[string][]int{}
	// The unit of each position: a file, or the index of a batch.
	mut unit_files := []string{cap: files.len}
	mut unit_batches := []int{cap: files.len}
	for file in files {
		candidate := test_batch_candidate(file, skipped) or {
			unit_files << file
			unit_batches << -1
			continue
		}
		mut target := -1
		for bi in open_batches[candidate.key] {
			batch := batches[bi]
			if batch.files.len < max_size && !candidate.names.any(it in batch.names) {
				target = bi
				break
			}
		}
		if target < 0 {
			target = batches.len
			batches << TestBatch{
				key: candidate.key
			}
			mut indexes := open_batches[candidate.key]
			indexes << target
			open_batches[candidate.key] = indexes
			unit_files << ''
			unit_batches << target
		}
		batches[target].files << file
		for name in candidate.names {
			batches[target].names[name] = true
		}
	}
	mut units := []string{cap: unit_files.len}
	for i, file in unit_files {
		bi := unit_batches[i]
		if bi < 0 {
			units << file
		} else {
			units << batches[bi].files.join('\n')
		}
	}
	return units
}

// test_batch_candidate returns the batch key (directory and module) and the
// top-level names of `file`, when it can share a binary with other tests.
fn test_batch_candidate(file string, skipped map[string]bool) ?TestBatchCandidate {
	name := os.file_name(file)
	// `.c.v`/`.js.v` tests select a backend, and `global` in a name enables globals.
	if !name.ends_with('_test.v') || name.all_before_last('_test.v').contains('.')
		|| name.contains('global') {
		return none
	}
	real_file := os.real_path(file)
	if real_file in skipped {
		return none
	}
	source := os.read_file(real_file) or { return none }
	for marker in test_batch_excluded_markers {
		if source.contains(marker) {
			return none
		}
	}
	mut module_name := test_file_module(source)
	if module_name in ['', 'main'] {
		for marker in test_batch_main_excluded_markers {
			if source.contains(marker) {
				return none
			}
		}
		module_name = 'main'
	}
	return TestBatchCandidate{
		key:   os.dir(real_file) + '\n' + module_name
		names: test_file_top_level_names(source)
	}
}

// test_file_module returns the module that `source` declares, or ''.
fn test_file_module(source string) string {
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed == '' || trimmed.starts_with('//') || trimmed.starts_with('@[') {
			continue
		}
		if trimmed.starts_with('module ') {
			return trimmed.all_after('module ').trim_space()
		}
		return ''
	}
	return ''
}

// test_file_top_level_names lists the declarations of `source` that would collide
// with a declaration of the same name in another file of the module: functions
// (methods as `Receiver.name`), constants, types and globals. Only declarations
// that start a line count; text in multi-line strings that looks like one only
// makes two files less likely to share a batch.
fn test_file_top_level_names(source string) []string {
	mut names := []string{}
	mut in_block := ''
	for raw_line in source.split_into_lines() {
		if in_block != '' {
			if raw_line.starts_with(')') {
				in_block = ''
				continue
			}
			trimmed := raw_line.trim_space()
			if trimmed == '' || trimmed.starts_with('//') {
				continue
			}
			ident := test_leading_ident(trimmed)
			if ident != '' {
				names << ident
			}
			continue
		}
		mut line := raw_line
		if line.starts_with('pub ') {
			line = line[4..]
		}
		mut keyword := ''
		for kw in ['fn ', 'const ', 'struct ', 'union ', 'enum ', 'type ', 'interface ', '__global '] {
			if line.starts_with(kw) {
				keyword = kw
				break
			}
		}
		if keyword == '' {
			continue
		}
		rest := line[keyword.len..].trim_space()
		if rest.starts_with('(') {
			if keyword == 'fn ' {
				// A method: `fn (mut r Receiver) name(`.
				receiver := rest.all_before(')').all_after_last(' ').trim_left('&')
				method := test_leading_ident(rest.all_after(')').trim_space())
				if method != '' {
					names << receiver + '.' + method
				}
			} else {
				in_block = keyword
			}
			continue
		}
		mut ident := test_leading_ident(rest)
		if keyword == 'fn ' && rest.starts_with('C.') {
			ident = 'C.' + test_leading_ident(rest[2..])
		}
		if ident != '' {
			names << ident
		}
	}
	return names
}

fn test_leading_ident(text string) string {
	mut end := 0
	for end < text.len && (text[end].is_letter() || text[end].is_digit() || text[end] == `_`) {
		end++
	}
	return text[..end]
}

// test_batch_output_head returns the first lines of `output` that are not notices
// or warnings, to name the failure of a batch.
fn test_batch_output_head(output string) string {
	mut lines := []string{}
	mut skipping := false
	for line in output.split_into_lines() {
		if line.contains(': notice: ') || line.contains(': warning: ') {
			skipping = true
			continue
		}
		if skipping && (line.starts_with(' ') || line.trim_space() == '') {
			continue
		}
		skipping = false
		lines << line
		if lines.len == 12 {
			break
		}
	}
	return lines.join('\n')
}

// test_label is how the result line of `file` names it.
fn (ts &TestSession) test_label(file string) string {
	mut relative_file := file
	if ts.root_relative {
		relative_file = relative_file.replace_once(ts.vroot + os.path_separator, '')
	}
	return relative_file.replace('\\', '/')
}

// run_test_batch compiles the test files `files` into one binary and runs it.
// When either fails, every file is run on its own, as without batching.
fn (mut ts TestSession) run_test_batch(files []string, test_id string, thread_id int, mut tls_bench benchmark.Benchmark) voidptr {
	abs_files := files.map(os.real_path(it))
	mtc := MessageThreadContext{
		file:    abs_files[0]
		flow_id: thread_id.str()
	}
	folder := os.join_path(ts.vtmp_dir, test_id)
	binary := os.join_path(folder, 'test_batch' + $if windows { '.exe' } $else { '' })
	mut passed := false
	// What failed, for VTEST_SHOW_CMD=1: a batch that only fails together points to
	// a compiler bug or to state that the tests share.
	mut failure := 'its folder could not be created'
	mut compile_duration := time.Duration(0)
	mut run_duration := time.Duration(0)
	if _ := os.mkdir_all(folder) {
		mut argv := ['-skip-running']
		vargs := (os.split_args(ts.vargs) or { []string{} }).filter(it != '-silent')
		argv << vargs
		if !vargs.any(it in ['-memory-limit', '-no-memory-limit']) {
			argv << ['-memory-limit', test_batch_memory_limit_mib().str()]
		}
		argv << ['-o', binary]
		argv << abs_files
		ts.append_message(.compile_begin, cmdexec.display(ts.vexe, argv), mtc)
		// A batch that fails is run again file by file: the compatibility compiler of
		// macOS must not retry it, and its diagnostics are not reported anyway.
		mut env := {
			'V_MACOS_V3_NO_FALLBACK': '1'
		}
		if ts.compiler_jobs > 0 && ts.compiler_jobs != runtime.nr_jobs() {
			env['VJOBS'] = ts.compiler_jobs.str()
		}
		compile_sw := time.new_stopwatch()
		compiled := cmdexec.run_in_merged_with_env(ts.vexe, argv, '', env)
		compile_duration = compile_sw.elapsed()
		ts.append_message_with_duration(.compile_end, compiled.output, compile_duration,
			mtc)
		if compiled.exit_code == 0 {
			ts.append_message(.cmd_begin, os.quoted_path(binary), mtc)
			run_sw := time.new_stopwatch()
			run := ts.exec([binary], mtc)
			run_duration = run_sw.elapsed()
			ts.append_message_with_duration(.cmd_end, run.output, run_duration, mtc)
			passed = run.exit_code == 0
			failure = 'it exited with ${run.exit_code}:\n${test_batch_output_head(run.output)}'
		} else {
			failure = 'it did not compile:\n${test_batch_output_head(compiled.output)}'
		}
		if ts.rm_binaries {
			os.rmdir_all(folder) or {}
		} else {
			os.rm(binary) or {}
			$if macos {
				os.rmdir_all(binary + '.dSYM') or {}
			}
		}
	}
	if !passed {
		if show_cmd {
			labels := abs_files.map(ts.test_label(it)).join(' ')
			ts.append_message(.info, '> the batch of ${files.len} test files failed; running them one by one: ${labels}\n> ${failure}',
				mtc)
		}
		for k, file in files {
			if ts.fail_fast && ts.has_failures() {
				break
			}
			ts.run_test_file(file, '${test_id}_${k}', thread_id, mut tls_bench)
		}
		return pool.no_result
	}
	compile_share := time.Duration(i64(compile_duration) / files.len)
	run_share := time.Duration(i64(run_duration) / files.len)
	for file in abs_files {
		ts.benchmark_step()
		tls_bench.step()
		ts.benchmark_ok()
		tls_bench.ok()
		if !hide_oks {
			ts.append_message_with_duration(.ok, tls_bench.step_message_with_label_and_duration(benchmark.b_ok,
				ts.test_label(file), run_share,
				preparation: compile_share
			), run_share, mtc)
		}
	}
	return pool.no_result
}
