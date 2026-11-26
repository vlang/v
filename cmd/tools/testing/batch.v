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
// file (but see test_batch_directives_allow), suite hooks run once per binary, and
// the rest change or depend on state of the whole process, which the other tests of
// a batch would share. A `fn main` of a module's internal test is an ordinary
// function of the module; one in two files collides by name.
const test_batch_excluded_markers = ['// v test', 'fn testsuite_begin', 'fn testsuite_end',
	'os.executable(', 'os.setenv(', 'os.unsetenv(', 'os.chdir(', 'os.args', 'arguments()', 'exit(']

// A `main` test is a program of its own. These change how all of a program is
// compiled or linked (C flags and headers, exported or global symbols), so a file
// with one of them is not built together with other programs.
const test_batch_main_excluded_markers = ['#flag', '#include', '#pkgconfig', '@[export', '__global',
	'@[noinit]', '@[console]', '@[windows_stdcall]']

struct TestBatchCandidate {
	key    string
	names  []string
	consts []string
	source string
}

struct TestBatch {
	key string
mut:
	files   []string
	names   map[string]bool
	consts  []string
	sources []string
}

// fits reports whether `candidate` can join the batch: no top-level name of one
// file is declared by another, and no constant of one is a word of another's
// source. A constant of the module conflicts with a local or an import alias of
// the same name in another file (`const it`, `const os`), which only shows up as
// a word there. A test function may share its name: the compiler renames that of
// a later file of a batch (`fn test_main()` in many files), as long as nothing in
// that file but its declaration names it.
fn (batch &TestBatch) fits(candidate TestBatchCandidate) bool {
	for name in candidate.names {
		if name in batch.names && !(name.starts_with('test_')
			&& candidate.source.count(name) == 1 && source_mentions_word(candidate.source, name)) {
			return false
		}
	}
	for name in candidate.consts {
		if batch.sources.any(source_mentions_word(it, name)) {
			return false
		}
	}
	for name in batch.consts {
		if source_mentions_word(candidate.source, name) {
			return false
		}
	}
	return true
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
	if ts.can_batch_tool_runs() {
		return ts.tool_units(files)
	}
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
		candidate := ts.test_batch_candidate(file, skipped) or {
			unit_files << file
			unit_batches << -1
			continue
		}
		mut target := -1
		for bi in open_batches[candidate.key] {
			batch := batches[bi]
			if batch.files.len < max_size && batch.fits(candidate) {
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
		batches[target].consts << candidate.consts
		batches[target].sources << candidate.source
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
fn (ts &TestSession) test_batch_candidate(file string, skipped map[string]bool) ?TestBatchCandidate {
	name := os.file_name(file)
	// `.js.v` and other suffixes select another backend or architecture. A `.c.v` test
	// runs with `-b c`, which is the backend of every batch (can_batch_tests).
	if !(name.ends_with('_test.v') || name.ends_with('_test.c.v'))
		|| name.all_before_last('_test.').contains('.') {
		return none
	}
	real_file := os.real_path(file)
	if real_file in skipped {
		return none
	}
	// Programs that a test writes out or compiles are string literals: what they
	// declare or call is not part of the test binary.
	original := os.read_file(real_file) or { return none }
	mut source := test_code_text(original)
	// A `testsuite_begin` that only unsets environment variables does the same when it
	// runs once for a whole batch of files that all have it (see test_env_cleanup_hook).
	mut hook := ''
	if cleanup := test_env_cleanup_hook(source, original) {
		source = source[..cleanup.start] + ' '.repeat(cleanup.end - cleanup.start) +
			source[cleanup.end..]
		hook = cleanup.text
	}
	// A file that sets environment variables can share a binary with others that do,
	// when it restores them the way these tests do (a saved `os.getenv_opt` and a
	// `defer`): a test of one file then finds the environment as the file before left it.
	restores_env := test_restores_environment(source)
	// The same for the working directory: it saves `os.getwd()` and has a `defer`.
	restores_cwd := source_contains_marker(source, 'os.chdir(')
		&& source_contains_marker(source, 'os.getwd(') && source_mentions_word(source, 'defer')
	for marker in test_batch_excluded_markers {
		if restores_env && marker in test_batch_environment_markers {
			continue
		}
		if restores_cwd && marker == 'os.chdir(' {
			continue
		}
		if source_contains_marker(source, marker) {
			return none
		}
	}
	if !ts.test_batch_directives_allow(source) {
		return none
	}
	mut module_name := test_file_module(source)
	if module_name in ['', 'main'] {
		for marker in test_batch_main_excluded_markers {
			if source.contains(marker) {
				return none
			}
		}
		// A test that says `module main` is compiled with the other `main` sources of
		// its directory, one without a module clause is not; their batches differ.
		module_name = if module_name == 'main' { 'main with its directory' } else { 'main' }
	}
	names := test_file_top_level_names(source)
	mut words := test_file_const_names(source)
	// A type named with one capital letter is spelled like a generic parameter of
	// another file (`struct T` and `fn f[T]()`), which then names the type.
	words << names.filter(it.len == 1 && it[0].is_capital())
	return TestBatchCandidate{
		key:    os.dir(real_file) + '\n' + module_name + if hook != '' { '\n' + hook } else { '' } +
			'\nvflags ' + test_file_vflags(source) + if restores_env { '\nenvironment' } else { '' } +
			if test_path_enables_globals(real_file) { '\nglobals' } else { '' } +
			if test_file_retries(source) { '\nretry' } else { '' } +
			if restores_cwd { '\nworking directory' } else { '' }
		names:  names
		consts: words
		source: source
	}
}

// test_file_retries reports whether `source` has a `// vtest retry:` line. Such tests failed
// now and then (most for a C toolchain race on Windows); a batch keeps them apart from the
// other files, so that a failure sends only them back to running one by one.
fn test_file_retries(source string) bool {
	return source.split_into_lines().any(it.starts_with('// vtest retry:'))
}

// test_path_enables_globals reports whether run_test_file compiles the file at the real
// path `path` with `-enable-globals`: when `global` is anywhere in that path.
fn test_path_enables_globals(path string) bool {
	return path.contains('global')
}

// test_batch_environment_markers are the excluded markers that test_restores_environment
// admits.
const test_batch_environment_markers = ['os.setenv(', 'os.unsetenv(']

// test_restores_environment reports whether the code text `source` changes environment
// variables and saves them to restore them: it reads them (`os.getenv_opt(` or `os.getenv(`)
// and has a `defer`.
fn test_restores_environment(source string) bool {
	if !test_batch_environment_markers.any(source_contains_marker(source, it)) {
		return false
	}
	return (source_contains_marker(source, 'os.getenv_opt(')
		|| source_contains_marker(source, 'os.getenv(')) && source_mentions_word(source, 'defer')
}

// test_file_vflags returns the flags of the last `// vtest vflags:` line of `source`, which
// get_test_details applies to a file that runs on its own.
fn test_file_vflags(source string) string {
	mut vflags := ''
	for line in source.split_into_lines() {
		if line.starts_with('// vtest vflags:') {
			vflags = line.all_after(':').trim_space()
		}
	}
	return vflags
}

struct TestEnvCleanupHook {
	start int
	end   int
	text  string
}

// test_env_cleanup_hook finds a `fn testsuite_begin` whose body only has lines like
// `os.unsetenv('VFLAGS')`, in the code text `code` of the file `original`. Files with the
// same such hook share a batch; the driver keeps the first file's hook for the binary
// (V3_INTERNAL_BATCH_SHARED_SUITE_HOOKS), which unsets the same variables before any test.
fn test_env_cleanup_hook(code string, original string) ?TestEnvCleanupHook {
	head := 'fn testsuite_begin() {\n'
	start := if code.starts_with(head) {
		0
	} else {
		(code.index('\n' + head) or { return none }) + 1
	}
	body_start := start + head.len
	body_end := code.index_after('\n}', body_start - 1) or { return none }
	mut lines := []string{}
	for line in original[body_start..body_end].split_into_lines() {
		text := line.trim_space()
		if text == '' {
			continue
		}
		if !text.starts_with("os.unsetenv('") || !text.ends_with("')") {
			return none
		}
		name := text["os.unsetenv('".len..text.len - 2]
		if name == '' || !name.bytes().all(it.is_letter() || it.is_digit() || it == `_`) {
			return none
		}
		lines << text
	}
	if lines.len == 0 {
		return none
	}
	return TestEnvCleanupHook{
		start: start
		end:   body_end + 2
		text:  lines.join('\n')
	}
}

// test_batch_directives_allow reports whether the `// vtest` directives of `source`
// let it share a binary. `// vtest build:` does, when its constraint holds: it
// decides whether the file runs, not how. One that does not hold leaves the file to
// run, and be skipped, on its own. `// vtest vflags:` does too: the batch key holds
// the flags (test_file_vflags), so a batch only has files compiled with the same ones.
// `retry:` files share batches only with each other; `flaky:` marks tests that fail
// now and then, which would send their whole batch back to running file by file.
fn (ts &TestSession) test_batch_directives_allow(source string) bool {
	if !source.contains('// vtest') {
		return true
	}
	for line in source.split_into_lines() {
		if !line.contains('// vtest') {
			continue
		}
		// `// vtest ownership` only decides whether the file runs at all, like `build:`.
		if line.starts_with('// vtest vflags:') || line == '// vtest ownership'
			|| line.starts_with('// vtest ownership ') || line.starts_with('// vtest ownership\t') {
			continue
		}
		// A `retry:` file batches only with other such files (test_file_retries), which
		// run one by one, with their retries, when their batch fails.
		if line.starts_with('// vtest retry:') {
			continue
		}
		if !line.starts_with('// vtest build:') {
			return false
		}
		holds := ts.build_environment.eval(line.all_after(':').trim_space()) or { false }
		if !holds {
			return false
		}
	}
	return true
}

// test_file_const_names lists the constants that `source` declares, also those of
// compile-time branches (`$if`, `$match`), which are indented.
fn test_file_const_names(source string) []string {
	mut names := []string{}
	mut in_block := false
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		if in_block {
			if trimmed.starts_with(')') {
				in_block = false
			} else if trimmed != '' && !trimmed.starts_with('//') {
				name := test_leading_ident(trimmed)
				if name != '' {
					names << name
				}
			}
			continue
		}
		mut rest := trimmed
		if rest.starts_with('pub ') {
			rest = rest[4..].trim_space()
		}
		if !rest.starts_with('const ') && !rest.starts_with('const(') {
			continue
		}
		rest = rest[5..].trim_space()
		if rest.starts_with('(') {
			in_block = true
			continue
		}
		name := test_leading_ident(rest)
		if name != '' {
			names << name
		}
	}
	return names
}

// source_contains_marker reports whether `marker` occurs in `source` where an
// identifier can start: `exit(` and `arguments()` are calls, while a test named
// `test_early_exit()` or `test_literal_arguments()` only ends with the same text.
fn source_contains_marker(source string, marker string) bool {
	mut start := 0
	for {
		idx := source.index_after(marker, start) or { return false }
		if idx == 0 || !is_test_ident_char(source[idx - 1]) {
			return true
		}
		start = idx + 1
	}
	return false
}

// source_mentions_word reports whether `word` occurs in `source` as a whole
// identifier.
fn source_mentions_word(source string, word string) bool {
	mut start := 0
	for {
		idx := source.index_after(word, start) or { return false }
		before_ok := idx == 0 || !is_test_ident_char(source[idx - 1])
		end := idx + word.len
		after_ok := end >= source.len || !is_test_ident_char(source[end])
		if before_ok && after_ok {
			return true
		}
		start = idx + 1
	}
	return false
}

// test_code_text returns `source` with the contents of its string literals
// replaced by spaces, keeping newlines, comments and the expressions of string
// interpolations. A misread literal only makes a batch fail to compile, after
// which its files run one by one.
fn test_code_text(source string) string {
	mut out := source.bytes()
	// Each open interpolation (`${`) is the brace depth inside it, and the string
	// that it continues, quote and all.
	mut interp_depths := []int{}
	mut interp_quotes := []u8{}
	mut quote := u8(0)
	mut raw := false
	mut i := 0
	for i < out.len {
		c := out[i]
		if quote != 0 {
			if !raw && c == `\\` && i + 1 < out.len {
				blank_test_text_byte(mut out, i)
				blank_test_text_byte(mut out, i + 1)
				i += 2
				continue
			}
			if c == quote {
				quote = 0
				i++
				continue
			}
			if !raw && c == `$` && i + 1 < out.len && out[i + 1] == `{` {
				interp_depths << 0
				interp_quotes << quote
				quote = 0
				i += 2
				continue
			}
			blank_test_text_byte(mut out, i)
			i++
			continue
		}
		if c == `/` && i + 1 < out.len && out[i + 1] == `/` {
			for i < out.len && out[i] != `\n` {
				i++
			}
			continue
		}
		if c == `/` && i + 1 < out.len && out[i + 1] == `*` {
			mut depth := 1
			i += 2
			for i < out.len && depth > 0 {
				if out[i] == `/` && i + 1 < out.len && out[i + 1] == `*` {
					depth++
					i += 2
				} else if out[i] == `*` && i + 1 < out.len && out[i + 1] == `/` {
					depth--
					i += 2
				} else {
					i++
				}
			}
			continue
		}
		if c == `\`` {
			// A rune literal, which can hold a quote.
			i++
			for i < out.len && out[i] != `\`` && out[i] != `\n` {
				if out[i] == `\\` {
					i++
				}
				i++
			}
			i++
			continue
		}
		if c == `'` || c == `"` {
			raw = i > 0 && out[i - 1] == `r` && (i == 1 || !is_test_ident_char(out[i - 2]))
			quote = c
			i++
			continue
		}
		if interp_depths.len > 0 {
			if c == `{` {
				interp_depths[interp_depths.len - 1]++
			} else if c == `}` {
				if interp_depths.last() == 0 {
					interp_depths.delete_last()
					quote = interp_quotes.pop()
					raw = false
				} else {
					interp_depths[interp_depths.len - 1]--
				}
			}
		}
		i++
	}
	return out.bytestr()
}

fn blank_test_text_byte(mut text []u8, i int) {
	if text[i] != `\n` {
		text[i] = ` `
	}
}

fn is_test_ident_char(c u8) bool {
	return c.is_letter() || c.is_digit() || c == `_`
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
// that start a line count; give it the text of test_code_text, so that the
// programs in a test's string literals do not count.
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
		// The files of a batch have the same `// vtest vflags:` (see test_file_vflags).
		if test_path_enables_globals(abs_files[0]) {
			argv << '-enable-globals'
		}
		batch_vflags := get_test_details(abs_files[0]).vflags
		if batch_vflags != '' {
			argv << os.split_args(batch_vflags) or { []string{} }
		}
		if !vargs.any(it in ['-memory-limit', '-no-memory-limit']) {
			argv << ['-memory-limit', test_batch_memory_limit_mib().str()]
		}
		argv << ['-o', binary]
		argv << abs_files
		ts.append_message(.compile_begin, cmdexec.display(ts.vexe, argv), mtc)
		mut env := map[string]string{}
		// The files of a batch have the same cleanup hook or none (test_env_cleanup_hook).
		env['V3_INTERNAL_BATCH_SHARED_SUITE_HOOKS'] = '1'
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
			run, timed_out := ts.exec_timed([binary], map[string]string{}, mtc)
			run_duration = run_sw.elapsed()
			ts.append_message_with_duration(.cmd_end, run.output, run_duration, mtc)
			passed = run.exit_code == 0
			failure = if timed_out {
				timed_out_message(ts.timeout_s)
			} else {
				'it exited with ${run.exit_code}:\n${test_batch_output_head(run.output)}'
			}
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
			if (ts.fail_fast && ts.has_failures()) || cmdexec.bounded_runs_cancelled() != 0 {
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

// `v vet`, `v fmt -verify` and `v fmt -inprocess -l` check every file they are given and name
// the ones they reject, so a session of one of them passes up to tool_batch_size files to one
// command instead of starting one (two, with the tool's own process) per file. A batch that
// fails runs again file by file.
const tool_batch_size = 64

// can_batch_tool_runs reports whether the session runs `v vet`, `v fmt -verify` or
// `v fmt -inprocess -l` over its files.
fn (ts &TestSession) can_batch_tool_runs() bool {
	if os.getenv(test_batch_disable_env) !in ['', '0'] || ts.show_stats || ts.build_tools
		|| ts.resume_dir != '' {
		return false
	}
	vargs := os.split_args(ts.vargs) or { return false }
	if 'vet' in vargs {
		return true
	}
	// `v test-fmt` runs `fmt -inprocess -l`.
	return 'fmt' in vargs && '-w' !in vargs
		&& ('-verify' in vargs || ('-inprocess' in vargs && '-l' in vargs))
}

// tool_units groups `files` for can_batch_tool_runs sessions. The files of a group get the same
// options from run_test_file: `vet` checks a file whose path has `global` with `-enable-globals`,
// and with its `// vtest vflags:`.
fn (ts &TestSession) tool_units(files []string) []string {
	is_fmt := ts.vargs.contains('fmt')
	mut groups := [][]string{}
	mut open_group := map[string]int{}
	mut unit_groups := []int{cap: files.len}
	for file in files {
		key := if is_fmt {
			''
		} else {
			real_file := os.real_path(file)
			'${test_path_enables_globals(real_file)} ${get_test_details(real_file).vflags}'
		}
		gi := open_group[key] or { -1 }
		if gi >= 0 && groups[gi].len < tool_batch_size {
			groups[gi] << file
			continue
		}
		open_group[key] = groups.len
		unit_groups << groups.len
		groups << [file]
	}
	return unit_groups.map(groups[it].join('\n'))
}

// run_tool_batch runs the tool of a can_batch_tool_runs session over `files` with one command.
// When it fails, every file is run on its own, so that the failures are reported as without
// batching.
fn (mut ts TestSession) run_tool_batch(files []string, test_id string, thread_id int, mut tls_bench benchmark.Benchmark) voidptr {
	abs_files := files.map(os.real_path(it))
	mtc := MessageThreadContext{
		file:    abs_files[0]
		flow_id: thread_id.str()
	}
	mut argv := [ts.vexe, '-skip-running']
	argv << (os.split_args(ts.vargs) or { []string{} }).filter(it != '-silent')
	if !ts.vargs.contains('fmt') {
		if test_path_enables_globals(abs_files[0]) {
			argv << '-enable-globals'
		}
		vflags := get_test_details(abs_files[0]).vflags
		if vflags != '' {
			argv << os.split_args(vflags) or { []string{} }
		}
	}
	argv << abs_files
	ts.append_message(.compile_begin, cmdexec.display(ts.vexe, argv[1..]), mtc)
	sw := time.new_stopwatch()
	res := ts.exec_compiler(argv, {}, mtc)
	duration := sw.elapsed()
	if res.exit_code != 0 {
		// The files run again one by one and print their own output.
		ts.append_message_with_duration(.compile_end, '', duration, mtc)
		if show_cmd {
			ts.append_message(.info, '> the batch of ${files.len} files failed; running them one by one\n> ${test_batch_output_head(res.output)}',
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
	ts.append_message_with_duration(.compile_end, res.output, duration, mtc)
	share := time.Duration(i64(duration) / files.len)
	for file in abs_files {
		ts.benchmark_step()
		tls_bench.step()
		ts.benchmark_ok()
		tls_bench.ok()
		if !hide_oks {
			ts.append_message_with_duration(.ok, tls_bench.step_message_with_label_and_duration(benchmark.b_ok,
				ts.test_label(file), 0,
				preparation: share
			), 0, mtc)
		}
	}
	return pool.no_result
}
