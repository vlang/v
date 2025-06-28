// Usage: v run run_bench.v
// It will generate a `result.md` in current dir.
import os
import arrays
import strings

const compilers = [
	'tcc',
	'clang',
	'gcc',
]

const run_iterations = 10

const nobj = 10000000
const run_settings = [
	[1, 1, 0],
	[1, 1, 100],
	[4, 4, 0],
	[4, 4, 100],
]

fn get_perf_from_result(result string) !f32 {
	lines := result.split_into_lines()
	for l in lines {
		if l.contains('objects') && l.contains('(') && l.contains(')') {
			f := l.find_between('(', ')').all_before('objs/µs').trim_space().f32()
			return f
		}
	}
	return error('run fail?')
}

fn main() {
	mut perf_result := []f32{}

	for cc in compilers {
		// 1. compile
		compile_cmd := 'v channel_bench_v.v -cc ${cc}'
		println('compile_cmd: ${compile_cmd}')
		compile_result := os.execute(compile_cmd)
		if compile_result.exit_code != 0 {
			panic('compile fail with "${compile_cmd}"')
		}

		// 2. run
		for s in run_settings {
			run_cmd := './channel_bench_v ${s[0]:-3} ${s[1]:-3} ${s[2]:-3} ${nobj}'
			println('-----------------------------------------------------------')
			mut iteration_result := []f32{}
			for i in 0 .. run_iterations {
				print('${i:3}: ${run_cmd}')
				run_result := os.execute(run_cmd)
				f := get_perf_from_result(run_result.output)!
				iteration_result << f
				println(' => ${f:.2} objs/µs')
			}
			avg := arrays.sum(iteration_result)! / run_iterations
			perf_result << avg
		}
	}

	// 3. output result
	mut sb := strings.new_builder(8192)
	sb.write_string('\n| nsend | nrec | buflen |')
	for cc in compilers {
		sb.write_string(' **V (${cc:-5})** |')
	}
	sb.writeln('')
	sb.write_string('| :---: | :---:| :---:  |')
	for _ in 0 .. compilers.len {
		sb.write_string('     :---:     |')
	}
	sb.writeln('')
	for i, s in run_settings {
		sb.write_string('|  ${s[0]:-3}  |  ${s[1]:-3} |   ${s[2]:-3}  |')
		for j in 0 .. compilers.len {
			sb.write_string('     ${perf_result[j * run_settings.len + i]:-5.2}     |')
		}
		sb.writeln('')
	}
	sb.writeln('')
	println('***********************************************************')
	println('writing result to `result.md`...')
	println('***********************************************************')
	os.write_file('result.md', sb.str())!
	println(sb.str())
	os.rm('./channel_bench_v')!
}
