module main

import os
import flag
import time
import scripting

struct CmdResult {
mut:
	runs int
	outputs []string
	oms map[string][]int
	timings []int
}
struct Context {
mut:
	count int
	warmup int
	show_help bool
	show_result bool
	verbose bool
	commands []string
	results []CmdResult
}

fn main(){
	mut context := Context{}
	context.parse_options()
	context.run()
	context.show_diff_summary()
	context.show_stddev()
}

fn (mut context Context) parse_options() {
	mut fp := flag.new_flag_parser(os.args)
	fp.application(os.file_name(os.executable()))
	fp.version('0.0.1')
	fp.description('Repeat command(s) and collect statistics. NB: you have to quote each command.')
	fp.arguments_description('CMD1 CMD2 ...')
	fp.skip_executable()
	fp.limit_free_args_to_at_least(1)
	context.count = fp.int('count', `c`, 10, 'Repetition count')
	context.warmup = fp.int('warmup', `w`, 2, 'Warmup runs')
	context.show_help = fp.bool('help', `h`, false, 'Show this help screen.')
	context.verbose = fp.bool('verbose', `v`, false, 'Be more verbose.')
	context.show_result = fp.bool('result', `r`, true, 'Show the result too.')
	if context.show_help {
		println(fp.usage())
		exit(0)
	}
	if context.verbose {
		scripting.set_verbose(true)
	}
	context.commands = fp.finalize() or {
		eprintln('Error: ' + err)
		exit(1)
	}
	context.results = []CmdResult{ len: context.commands.len, init: CmdResult{} }
}

fn (mut context Context) run() {
	print('')
	for icmd, cmd in context.commands {
		mut runs := 0
		mut duration := 0
		mut sum := 0
		mut oldres := ''
		if context.warmup > 0 {
			for i in 1..context.warmup+1 {
				print('\r warming up run: ${i:4}/${context.warmup:-4} for ${cmd:-50s} took ${duration:6} ms ...')
				mut sw := time.new_stopwatch({})
				os.exec(cmd) or { continue }
				duration = int(sw.elapsed().milliseconds())
			}
		}
		for i in 1..(context.count+1) {
			avg := f64(sum)/f64(i)
			print('\r average: ${avg:9.3f} ms | run: ${i:4}/${context.count:-4} | took ${duration:6} ms | cmd: ${cmd:-50s}')
			if context.show_result {
				print(' | result: ${oldres:-s}')
			}
			mut sw := time.new_stopwatch({})
			res := os.exec(cmd) or {
				eprintln('${i:10} failed runnning cmd: $cmd')
				continue
			}
			duration = int(sw.elapsed().milliseconds())
			if res.exit_code != 0 {
				eprintln('${i:10} non 0 exit code for cmd: $cmd')
				continue
			}
			context.results[icmd].outputs << res.output.trim_right('\r\n').replace('\r\n', '\n').split("\n")
			context.results[icmd].timings << duration
			sum += duration
			runs++
			oldres = res.output.replace('\n', ' ')
		}
		context.results[icmd].runs = runs
		println('')
	}
	for icmd in 0..context.results.len{
		mut m := map[string][]int
		for o in context.results[icmd].outputs {
			x := o.split(':')
			if x.len > 1 {
				k := x[0]
				v := x[1].trim_left(' ').int()
				m[k] << v
			}
		}
		context.results[icmd].oms = m
	}
}

fn (mut context Context) show_diff_summary() {
	// TODO
	//eprintln("$context.results")
}
fn (mut context Context) show_stddev() {
	// TODO
}
