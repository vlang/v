import os

fn test_raw_profile_report_v_policy_with_host_descriptors() ! {
	$if !amd64 || windows {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'profile_v_report_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	// Exercise the actual V report policy on hosted descriptors. Target-only
	// inclusion is checked separately with unmodified cross-target generation.
	source := os.read_file(os.join_path(@VEXEROOT, 'vlib/v/profile/report_d_freestanding.v'))!
	assert source.contains('$if macos && amd64 && !native {')
	os.write_file(os.join_path(work, 'report.v'), source.replace_once('module profile', 'module main').replace_once('$if macos && amd64 && !native {', '$if amd64 {'))!
	fixtures := os.join_path(@VEXEROOT, 'vlib/v/compiler_tests/testdata/profile_report')
	for name in ['main.c.v', 'format_oracle.c', 'format_oracle.h'] {
		os.cp(os.join_path(fixtures, name), os.join_path(work, name))!
	}
	for prod in [false, true] {
		binary := os.join_path(work, 'report')
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', cc, '-o', binary,
			work]
		if prod { command << '-prod' }
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := os.exec([binary, os.join_path(work, 'report.txt')])
		assert ran.exit_code == 0, ran.output
		assert ran.output.contains(' main__stdout \n'), ran.output
		assert ran.output.trim_space().ends_with('V profile report writer passed'), ran.output
	}
}

fn profile_report_function_body(source string, name string) string {
	definitions := source.split_into_lines().filter(it.contains(' ${name}(') && it.ends_with(' {'))
	assert definitions.len == 1, '${name}: ${definitions}'
	return source.all_after(definitions[0]).all_before('\n}\n')
}

fn compile_profile_report_fragment(source string, runtime string, work string, prod bool) ! {
	clang := os.find_abs_path_of_executable('clang') or { return }
	start := source.index('// V profile counters:') or { panic('profile counters missing') }
	function := 'void vprint_profile_stats(void) {'
	end := source.index(function) or { panic('profile report missing') }
	support := source[start..end] + function + profile_report_function_body(source, 'vprint_profile_stats') + '\n}\n'
	// Keep the actual generated prototypes on the same side of the report body.
	// This must fail C compilation when generation puts a helper call first.
	mut before := ''
	mut after := ''
	for name in ['begin_report', 'write_report_row', 'end_report'] {
		declarations := source.split_into_lines().filter(!it.starts_with('\t') && it.contains(' ${runtime}__${name}(') && it.ends_with(';'))
		assert declarations.len == 1, '${name}: ${declarations}'
		declaration := declarations[0]
		position := source.index(declaration) or { panic(declaration) }
		if position < start { before += declaration + '\n' } else { after += declaration + '\n' }
	}
	rows := profile_report_function_body(source, 'vprint_profile_stats').split_into_lines().filter(it.contains('if (') && it.contains('__write_report_row('))
	assert rows.len > 0
	calls := rows[0].all_after('if (').all_before(')')
	counter := calls.all_before_last('_calls')
	traced := support.contains('_v_profile_previous_trace')
	fixture := '#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
typedef uint8_t u8;
typedef uint64_t u64;
typedef struct { u8* str; int64_t len; int is_lit; } string;
typedef struct { int open; } ${runtime}__Report;
bool trace_calls__g_trace_in_hook;
static int opened, written, closed;
${before}
${support}
${after}
${runtime}__Report ${runtime}__begin_report(string path) {
 assert(path.len > 0 && path.str != 0);
 assert(trace_calls__g_trace_in_hook == ${if traced { 'true' } else { 'false' }});
 opened++;
 return (${runtime}__Report){1};
}
void ${runtime}__write_report_row(${runtime}__Report* report, u64 calls, double total, double own, string name) {
 assert(report->open && calls == 3 && total == 9 && own == 6);
 assert(name.len > 0 && name.str != 0);
 written++;
}
void ${runtime}__end_report(${runtime}__Report* report) {
 assert(report->open);
 report->open = 0;
 closed++;
}
int main(void) {
 (void)prof_measured_time;
 ${calls} = 3;
 ${counter} = 9;
 ${counter}_only_current = 6;
 vprint_profile_stats();
 assert(opened == 1 && written == 1 && closed == 1);
 assert(!trace_calls__g_trace_in_hook);
 return 0;
}
'
	c_source := os.join_path(work, 'profile_fragment.c')
	binary := os.join_path(work, 'profile_fragment')
	os.write_file(c_source, fixture)!
	compiled := os.exec([clang, '-std=c11', '-Wall', '-Wextra', '-Werror',
		if prod { '-O2' } else { '-O0' }, c_source, '-o', binary])
	assert compiled.exit_code == 0, compiled.output
	ran := os.exec([binary])
	assert ran.exit_code == 0, ran.output
}

fn test_raw_profile_and_trace_generation_keeps_hosted_and_arm64_paths() ! {
	work := os.join_path(os.vtmp_dir(), 'profile_report_codegen_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(work, 'main.v')
	os.write_file(source, "fn main() { println('profile report candidate') }\n")!
	for prod in [false, true] {
		for target in ['raw', 'raw_trace', 'hosted', 'arm64'] {
			output := os.join_path(work, 'report.c')
			mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', 'clang', '-os',
				'macos', '-arch', if target == 'arm64' { 'arm64' } else { 'amd64' }, '-profile',
				os.join_path(work, 'profile "report".txt'), '-o', output]
			if prod { command << '-prod' }
			if target != 'hosted' { command << '-freestanding' }
			if target == 'raw_trace' { command << '-trace-calls' }
			command << source
			built := os.exec(command)
			assert built.exit_code == 0, built.output
			code := os.read_file(output)!
			report := profile_report_function_body(code, 'vprint_profile_stats')
			if target.starts_with('raw') {
				compile_profile_report_fragment(code, 'profile', work, prod)!
				assert report.contains('profile__begin_report('), report
				assert report.contains('profile__write_report_row('), report
				assert report.contains('profile__end_report('), report
				for symbol in ['fopen(', 'fprintf(', 'printf(', 'fclose(', 'FILE*'] {
					assert !report.contains(symbol), report
				}
				for name in ['begin_report', 'write_report_row', 'append_report_float', 'end_report'] {
					body := profile_report_function_body(code, 'profile__${name}')
					assert !body.contains('_PROF_') && !body.contains('trace_calls__on_call('), body
				}
				if target == 'raw_trace' {
					for name in ['on_call', 'initialize_trace_clock_and_header'] {
						body := profile_report_function_body(code, 'trace_calls__${name}')
						assert !body.contains('fprintf(') && !body.contains('fflush('), body
						assert body.contains(if name == 'on_call' {
							'write_raw_trace_row('
						} else {
							'eprintln('
						}), body
					}
					assert report.contains('g_trace_in_hook = true;'), report
					assert report.contains('g_trace_in_hook = _v_profile_previous_trace;'), report
				}
			} else {
				assert !report.contains('profile__begin_report('), report
				assert report.contains('fopen(') && report.contains('fprintf(') && report.contains('fclose('), report
			}
			// Reporting still registers both signal callbacks and normal-exit output.
			assert code.contains('v_signal_with_handler_cast(SIGINT,'), target
			assert code.contains('v_signal_with_handler_cast(SIGTERM,'), target
			assert code.contains('atexit(vprint_profile_stats);'), target
		}
	}
}

fn test_raw_profile_runtime_remains_distinct_from_user_profile_module() ! {
	work := os.join_path(os.vtmp_dir(), 'profile_report_namespace_${os.getpid()}')
	os.mkdir_all(os.join_path(work, 'profile'))!
	defer { os.rmdir_all(work) or {} }
	os.write_file(os.join_path(work, 'v.mod'), "Module { name: 'report_namespace' }\n")!
	os.write_file(os.join_path(work, 'profile/profile.v'), 'module profile\npub fn sample() int { return 7 }\n')!
	source := os.join_path(work, 'main.v')
	os.write_file(source, 'import profile\nfn main() { assert profile.sample() == 7 }\n')!
	output := os.join_path(work, 'report.c')
	built := os.exec([@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', 'clang', '-os', 'macos',
		'-arch', 'amd64', '-freestanding', '-profile', '-', '-o', output, source])
	assert built.exit_code == 0, built.output
	code := os.read_file(output)!
	report := profile_report_function_body(code, 'vprint_profile_stats')
	assert report.contains('v__profile__Report _v_profile_report'), report
	for name in ['begin_report', 'write_report_row', 'end_report'] {
		assert report.contains('v__profile__${name}('), report
		assert profile_report_function_body(code, 'v__profile__${name}').len > 0, name
	}
	compile_profile_report_fragment(code, 'v__profile', work, false)!
}
