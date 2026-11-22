module main

import os
import v.cmdexec

fn test_sum_storage_acquires_only_active_literal_and_shares_cleanup() {
	// A Windows `clang` usually targets the MSVC ABI, which V cannot build Windows
	// programs with, so use the MinGW gcc there.
	cc := os.find_abs_path_of_executable($if windows { 'gcc' } $else { 'clang' }) or { return }
	dir := os.join_path(os.vtmp_dir(), 'sum_storage_cleanup_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	compiler := if os.getenv('VTOOLS_NO_CACHE').trim_space().to_lower() in ['1', 'true', 'yes',
		'on'] {
		project := os.join_path(os.real_path(dir), 'compiler')
		vlib := os.join_path(@VEXEROOT, 'vlib')
		// Release V's frontend before Clang compiles the private driver.
		generate := os.exec([@VEXE, '-new-compiler', '-cc', cc, '-gc', 'none', '-d', 'ownership',
			'-path', vlib, '-generate-c-project', project, os.join_path(vlib, 'v', 'v.v')])
		assert generate.exit_code == 0, generate.output
		build := $if windows {
			cmdexec.run_in_merged('cmd', ['/c', 'build.bat'], project)
		} $else {
			cmdexec.run_in_merged('sh', [os.join_path(project, 'build.sh')], project)
		}
		assert build.exit_code == 0, build.output
		binary := os.join_path(project, 'v')
		if os.is_file(binary) { binary } else { binary + '.exe' }
	} else {
		@VEXE
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'type Choice = int | string | []int | []string | Box[[2]string]
type Nested = Choice | bool
type Box[T] = T | int
type ChoiceAlias = Choice
@[packed]
struct PackedChoice {
 prefix u8
mut:
 choice Choice
}
@[packed]
struct PackedAlias {
 prefix u8
mut:
 choice ChoiceAlias
}
fn replacement_index(mut calls []int) int {
 calls[0]++
 return 0
}
fn boxed() Box[[2]string] { return 17 }
fn scalar() Choice { return Choice(17) }
fn slice_value() Choice {
	values := [1, 2, 3]
	return Choice(values[1..])
}
fn nested_slice() Nested {
	values := ["first", "second"]
	return Nested(Choice(values[1..]))
}
fn unknown(value Choice) Choice { return value }
fn acquired() Choice { return unknown(Choice([4, 5])) }
fn unknown_nested(value Nested) Nested { return value }
fn acquired_nested() Nested { return unknown_nested(nested_slice()) }
fn main() {
	mut packed := PackedChoice{prefix: 3, choice: Choice("old".clone())}
	packed.choice = Choice(19)
	assert (packed.choice as int) == 19
	packed.choice = Choice("replacement".clone())
	assert (packed.choice as string) == "replacement"
	assert packed.prefix == 3
	mut alias_value := PackedAlias{prefix: 5, choice: ChoiceAlias(Choice("alias".clone()))}
	alias_value.choice = ChoiceAlias(Choice(23))
	assert (Choice(alias_value.choice) as int) == 23
	assert alias_value.prefix == 5
	mut items := [PackedChoice{prefix: 7, choice: Choice("indexed".clone())}]
	mut calls := [0]
	items[replacement_index(mut calls)].choice = Choice(29)
	assert calls[0] == 1
	assert items[0].prefix == 7 && (items[0].choice as int) == 29
	spawn boxed()
	assert (boxed() as int) == 17
	assert (scalar() as int) == 17
	assert (slice_value() as []int) == [2, 3]
	nested := nested_slice() as Choice
	assert (nested as []string) == ["second"]
	assert (acquired() as []int) == [4, 5]
	dynamic := acquired_nested() as Choice
	assert (dynamic as []string) == ["second"]
	assert (unknown(Choice("text")) as string) == "text"
}')!
	binary := os.join_path(dir, 'program')
	// Older gcc has no `-fsanitize-trap=`, and without trap mode gcc links
	// libubsan, which MinGW does not ship.
	sanitize := $if windows {
		'-fsanitize=alignment -fsanitize-undefined-trap-on-error'
	} $else {
		'-fsanitize=alignment -fsanitize-trap=alignment'
	}
	for jobs in ['1', '2'] {
		for mode in ['-ownership', '-autofree'] {
			// The explicit C backend retains the translation unit used for the alignment check.
			run := cmdexec.run_in_merged_with_env(compiler, ['-new-compiler', '-nocache', '-b',
				'c', '-gc', 'none', '-cc', cc, '-cflags', sanitize, mode, '-o', binary, 'run',
				source], dir, {
				'VJOBS': jobs
			})
			assert run.exit_code == 0, 'VJOBS=${jobs} ${mode}: ${run.output}'
			csrc := os.read_file('${binary}.c')!
			// The known scalar payload must bypass unrelated array variants entirely.
			assert csrc.contains('scalar(void) {')
			scalar := csrc.all_after('scalar(void) {').all_before('\n}')
			assert scalar.len > 0
			assert !scalar.contains('owned_sum_array_escape'), scalar
			assert !scalar.contains('is_slice_view'), scalar
			// Serial and parallel support lanes must both enroll transformed cleanup sites.
			lines := csrc.split_into_lines()
			helpers := lines.filter(it.starts_with('static void __v3_ownership_drop_'))
			assert helpers.filter(it.contains('Choice(')).len == 2, 'VJOBS=${jobs} ${mode}: ${helpers}'
			assert helpers.filter(it.contains('Nested(')).len == 2, 'VJOBS=${jobs} ${mode}: ${helpers}'
			sum_header := lines.filter(it.starts_with('struct Box'))
			assert sum_header.len == 1, sum_header.str()
			sum_c_type := sum_header[0].all_after('struct ').all_before(' {')
			assert helpers.filter(it.contains('(${sum_c_type}* _value)')).len == 2, 'VJOBS=${jobs} ${mode}: ${helpers}'
			detached := lines.filter(it.starts_with('static void __v3_detached_ownership_drop_'))
			assert detached.filter(it.contains('(${sum_c_type}* _value)')).len == 2, 'VJOBS=${jobs} ${mode}: ${detached}'
		}
	}
}
