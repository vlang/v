module main

import os
import flag
import time
import term
import math
import scripting
import v.util

struct CmdResult {
mut:
	runs    int
	cmd     string
	icmd    int
	outputs []string
	oms     map[string][]int
	summary map[string]Aints
	timings []int
	atiming Aints
}

struct Context {
mut:
	count                   int
	series                  int
	warmup                  int
	show_help               bool
	show_output             bool
	fail_on_regress_percent int
	fail_on_maxtime         int // in ms
	verbose                 bool
	commands                []string
	results                 []CmdResult
	cmd_template            string // {T} will be substituted with the current command
	cmd_params              map[string][]string
	cline                   string // a terminal clearing line
}

struct Aints {
	values  []int
mut:
	imin    int
	imax    int
	average f64
	stddev  f64
}

fn new_aints(vals []int) Aints {
	mut res := Aints{
		values: vals
	}
	mut sum := i64(0)
	mut imin := math.max_i32
	mut imax := -math.max_i32
	for i in vals {
		sum += i
		if i < imin {
			imin = i
		}
		if i > imax {
			imax = i
		}
	}
	res.imin = imin
	res.imax = imax
	if vals.len > 0 {
		res.average = sum / f64(vals.len)
	}
	//
	mut devsum := f64(0.0)
	for i in vals {
		x := f64(i) - res.average
		devsum += (x * x)
	}
	res.stddev = math.sqrt(devsum / f64(vals.len))
	return res
}

fn (a Aints) str() string {
	return util.bold('${a.average:9.3f}') +
		'ms ± σ: ${a.stddev:-5.1f}ms, min … max: ${a.imin}ms … ${a.imax}ms'
}

const (
	max_fail_percent             = 100000
	max_time                     = 60 * 1000 // ms
	performance_regression_label = 'Performance regression detected, failing since '
)

fn main() {
	mut context := Context{}
	context.parse_options()
	context.run()
	context.show_diff_summary()
}

fn (mut context Context) parse_options() {
	mut fp := flag.new_flag_parser(os.args)
	fp.application(os.file_name(os.executable()))
	fp.version('0.0.1')
	fp.description('Repeat command(s) and collect statistics. NB: you have to quote each command, if it contains spaces.')
	fp.arguments_description('CMD1 CMD2 ...')
	fp.skip_executable()
	fp.limit_free_args_to_at_least(1)
	context.count = fp.int('count', `c`, 10, 'Repetition count.')
	context.series = fp.int('series', `s`, 2, 'Series count. `-s 2 -c 4 a b` => aaaabbbbaaaabbbb, while `-s 3 -c 2 a b` => aabbaabbaabb.')
	context.warmup = fp.int('warmup', `w`, 2, 'Warmup runs. These are done *only at the start*, and are ignored.')
	context.show_help = fp.bool('help', `h`, false, 'Show this help screen.')
	context.show_output = fp.bool('output', `O`, false, 'Show command stdout/stderr in the progress indicator for each command. NB: slower, for verbose commands.')
	context.verbose = fp.bool('verbose', `v`, false, 'Be more verbose.')
	context.fail_on_maxtime = fp.int('max_time', `m`, max_time, 'Fail with exit code 2, when first cmd takes above M milliseconds (regression).')
	context.fail_on_regress_percent = fp.int('fail_percent', `f`, max_fail_percent, 'Fail with exit code 3, when first cmd is X% slower than the rest (regression).')
	context.cmd_template = fp.string('template', `t`, '{T}', 'Command template. {T} will be substituted with the current command.')
	cmd_params := fp.string_multi('parameter', `p`, 'A parameter substitution list. `{p}=val1,val2,val2` means that {p} in the template, will be substituted with each of val1, val2, val3.')
	for p in cmd_params {
		parts := p.split(':')
		if parts.len > 1 {
			context.cmd_params[parts[0]] = parts[1].split(',')
		}
	}
	if context.show_help {
		println(fp.usage())
		exit(0)
	}
	if context.verbose {
		scripting.set_verbose(true)
	}
	commands := fp.finalize() or {
		eprintln('Error: ' + err)
		exit(1)
	}
	context.commands = context.expand_all_commands(commands)
	context.results = []CmdResult{len: context.commands.len, init: CmdResult{}}
	context.cline = '\r' + term.h_divider('')
}

fn (mut context Context) clear_line() {
	print(context.cline)
}

fn (mut context Context) expand_all_commands(commands []string) []string {
	mut all_commands := []string{}
	for cmd in commands {
		maincmd := context.cmd_template.replace('{T}', cmd)
		mut substituted_commands := [maincmd]
		for paramk, paramlist in context.cmd_params {
			for paramv in paramlist {
				mut new_substituted_commands := []string{}
				for cscmd in substituted_commands {
					scmd := cscmd.replace(paramk, paramv)
					new_substituted_commands << scmd
				}
				substituted_commands << new_substituted_commands
			}
		}
		all_commands << substituted_commands
	}
	mut unique := map[string]int{}
	for x in all_commands {
		if x.contains('{') && x.contains('}') {
			continue
		}
		unique[x] = 1
	}
	return unique.keys()
}

fn (mut context Context) run() {
	mut run_warmups := 0
	for si in 1 .. context.series + 1 {
		for icmd, cmd in context.commands {
			mut runs := 0
			mut duration := 0
			mut sum := 0
			mut oldres := ''
			println('Series: ${si:4}/${context.series:-4}, command: $cmd')
			if context.warmup > 0 && run_warmups < context.commands.len {
				for i in 1 .. context.warmup + 1 {
					print('\r warming up run: ${i:4}/${context.warmup:-4} for ${cmd:-50s} took ${duration:6} ms ...')
					mut sw := time.new_stopwatch({})
					os.exec(cmd) or {
						continue
					}
					duration = int(sw.elapsed().milliseconds())
				}
				run_warmups++
			}
			context.clear_line()
			for i in 1 .. (context.count + 1) {
				avg := f64(sum) / f64(i)
				print('\rAverage: ${avg:9.3f}ms | run: ${i:4}/${context.count:-4} | took ${duration:6} ms')
				if context.show_output {
					print(' | result: ${oldres:s}')
				}
				mut sw := time.new_stopwatch({})
				res := scripting.exec(cmd) or {
					continue
				}
				duration = int(sw.elapsed().milliseconds())
				if res.exit_code != 0 {
					eprintln('${i:10} non 0 exit code for cmd: $cmd')
					continue
				}
				context.results[icmd].outputs <<
					res.output.trim_right('\r\n').replace('\r\n', '\n').split('\n')
				context.results[icmd].timings << duration
				sum += duration
				runs++
				oldres = res.output.replace('\n', ' ')
			}
			context.results[icmd].cmd = cmd
			context.results[icmd].icmd = icmd
			context.results[icmd].runs += runs
			context.results[icmd].atiming = new_aints(context.results[icmd].timings)
			context.clear_line()
			print('\r')
			mut m := map[string][]int{}
			for o in context.results[icmd].outputs {
				x := o.split(':')
				if x.len > 1 {
					k := x[0]
					v := x[1].trim_left(' ').int()
					m[k] << v
				}
			}
			mut summary := map[string]Aints{}
			for k, v in m {
				// show a temporary summary for the current series/cmd cycle
				s := new_aints(v)
				println('  $k: $s')
				summary[k] = s
			}
			// merge current raw results to the previous ones
			old_oms := context.results[icmd].oms
			mut new_oms := map[string][]int{}
			for k, v in m {
				if old_oms[k].len == 0 {
					new_oms[k] = v
				} else {
					new_oms[k] << old_oms[k]
					new_oms[k] << v
				}
			}
			context.results[icmd].oms = new_oms
			// println('')
		}
	}
	// create full summaries, taking account of all runs
	for icmd in 0 .. context.results.len {
		mut new_full_summary := map[string]Aints{}
		for k, v in context.results[icmd].oms {
			new_full_summary[k] = new_aints(v)
		}
		context.results[icmd].summary = new_full_summary
	}
}

fn (mut context Context) show_diff_summary() {
	context.results.sort_with_compare(fn (a &CmdResult, b &CmdResult) int {
		if a.atiming.average < b.atiming.average {
			return -1
		}
		if a.atiming.average > b.atiming.average {
			return 1
		}
		return 0
	})
	println('Summary (commands are ordered by ascending mean time), after $context.series series of $context.count repetitions:')
	base := context.results[0].atiming.average
	mut first_cmd_percentage := f64(100.0)
	for i, r in context.results {
		cpercent := (r.atiming.average / base) * 100 - 100
		first_marker := if r.icmd == 0 { util.bold('>') } else { ' ' }
		if r.icmd == 0 {
			first_cmd_percentage = cpercent
		}
		println(' $first_marker${(i + 1):3} | ${cpercent:6.1f}% slower | ${r.cmd:-55s} | $r.atiming')
	}
	$if debugcontext ? {
		println('context: $context')
	}
	if int(base) > context.fail_on_maxtime {
		print(performance_regression_label)
		println('average time: ${base:6.1f} ms > $context.fail_on_maxtime ms threshold.')
		exit(2)
	}
	if context.fail_on_regress_percent == max_fail_percent || context.results.len < 2 {
		return
	}
	fail_threshold_max := f64(context.fail_on_regress_percent)
	if first_cmd_percentage > fail_threshold_max {
		print(performance_regression_label)
		println('${first_cmd_percentage:5.1f}% > ${fail_threshold_max:5.1f}% threshold.')
		exit(3)
	}
}
