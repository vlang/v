module main

import os
import flag
import time
import term
import math
import scripting

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
	use_newline             bool // use \n instead of \r, so the last line is not overwritten
	fail_on_regress_percent int
	fail_on_maxtime         int // in ms
	verbose                 bool
	commands                []string
	results                 []CmdResult
	cmd_template            string // {T} will be substituted with the current command
	cmd_params              map[string][]string
	cline                   string // a terminal clearing line
	cgoback                 string
	nmins                   int // number of minimums to discard
	nmaxs                   int // number of maximums to discard
}

[unsafe]
fn (mut result CmdResult) free() {
	unsafe {
		result.cmd.free()
		result.outputs.free()
		result.oms.free()
		result.summary.free()
		result.timings.free()
		result.atiming.free()
	}
}

[unsafe]
fn (mut context Context) free() {
	unsafe {
		context.commands.free()
		context.results.free()
		context.cmd_template.free()
		context.cmd_params.free()
		context.cline.free()
		context.cgoback.free()
	}
}

struct Aints {
	values []int
mut:
	imin    int
	imax    int
	average f64
	stddev  f64
	nmins   int // number of discarded fastest results
	nmaxs   int // number of discarded slowest results
}

[unsafe]
fn (mut a Aints) free() {
	unsafe { a.values.free() }
}

fn new_aints(ovals []int, extreme_mins int, extreme_maxs int) Aints {
	mut res := Aints{
		values: ovals // remember the original values
		nmins: extreme_mins
		nmaxs: extreme_maxs
	}
	mut sum := i64(0)
	mut imin := math.max_i32
	mut imax := -math.max_i32
	// discard the extremes:
	mut vals := []int{}
	for x in ovals {
		vals << x
	}
	vals.sort()
	if vals.len > extreme_mins + extreme_maxs {
		vals = vals[extreme_mins..vals.len - extreme_maxs].clone()
	} else {
		vals = []
	}
	// statistical processing of the remaining values:
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
	// eprintln('\novals: $ovals\n vals: $vals\n vals.len: $vals.len |  res.imin: $res.imin | res.imax: $res.imax | res.average: $res.average | res.stddev: $res.stddev')
	return res
}

fn bold(s string) string {
	return term.colorize(term.bold, s)
}

fn (a Aints) str() string {
	return bold('${a.average:6.2f}') +
		'ms ± σ: ${a.stddev:4.1f}ms, min: ${a.imin:4}ms, max: ${a.imax:4}ms, runs:${a.values.len:3}, nmins:${a.nmins:2}, nmaxs:${a.nmaxs:2}'
}

const (
	max_fail_percent             = 100 * 1000
	max_time                     = 60 * 1000 // ms
	performance_regression_label = 'Performance regression detected, failing since '
)

fn main() {
	mut context := Context{}
	context.parse_options()!
	context.run()
	context.show_diff_summary()
}

fn (mut context Context) parse_options() ! {
	mut fp := flag.new_flag_parser(os.args)
	fp.application(os.file_name(os.executable()))
	fp.version('0.0.1')
	fp.description('Repeat command(s) and collect statistics. Note: you have to quote each command, if it contains spaces.')
	fp.arguments_description('CMD1 CMD2 ...')
	fp.skip_executable()
	fp.limit_free_args_to_at_least(1)!
	context.count = fp.int('count', `c`, 10, 'Repetition count.')
	context.series = fp.int('series', `s`, 2, 'Series count. `-s 2 -c 4 a b` => aaaabbbbaaaabbbb, while `-s 3 -c 2 a b` => aabbaabbaabb.')
	context.warmup = fp.int('warmup', `w`, 2, 'Warmup runs. These are done *only at the start*, and are ignored.')
	context.show_help = fp.bool('help', `h`, false, 'Show this help screen.')
	context.use_newline = fp.bool('newline', `n`, false, 'Use \\n, do not overwrite the last line. Produces more output, but easier to diagnose.')
	context.show_output = fp.bool('output', `O`, false, 'Show command stdout/stderr in the progress indicator for each command. Note: slower, for verbose commands.')
	context.verbose = fp.bool('verbose', `v`, false, 'Be more verbose.')
	context.fail_on_maxtime = fp.int('max_time', `m`, max_time, 'Fail with exit code 2, when first cmd takes above M milliseconds (regression).')
	context.fail_on_regress_percent = fp.int('fail_percent', `f`, max_fail_percent, 'Fail with exit code 3, when first cmd is X% slower than the rest (regression).')
	context.cmd_template = fp.string('template', `t`, r'{T}', r'Command template. {T} will be substituted with the current command.')
	cmd_params := fp.string_multi('parameter', `p`, r'A parameter substitution list. `{p}=val1,val2,val2` means that {p} in the template, will be substituted with each of val1, val2, val3.')
	context.nmins = fp.int('nmins', `i`, 0, 'Ignore the BOTTOM X results (minimum execution time). Makes the results more robust to performance flukes.')
	context.nmaxs = fp.int('nmaxs', `a`, 1, 'Ignore the TOP X results (maximum execution time). Makes the results more robust to performance flukes.')
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
		eprintln('Error: $err')
		exit(1)
	}
	context.commands = context.expand_all_commands(commands)
	context.results = []CmdResult{len: context.commands.len, cap: 20, init: CmdResult{
		outputs: []string{cap: 500}
		timings: []int{cap: 500}
	}}
	if context.use_newline {
		context.cline = '\n'
		context.cgoback = '\n'
	} else {
		context.cline = '\r' + term.h_divider('')
		context.cgoback = '\r'
	}
}

fn flushed_print(s string) {
	print(s)
	flush_stdout()
}

fn (mut context Context) clear_line() {
	flushed_print(context.cline)
}

fn (mut context Context) expand_all_commands(commands []string) []string {
	mut all_commands := []string{}
	for cmd in commands {
		maincmd := context.cmd_template.replace(r'{T}', cmd)
		mut substituted_commands := []string{}
		substituted_commands << maincmd
		for paramk, paramlist in context.cmd_params {
			for paramv in paramlist {
				mut new_substituted_commands := []string{}
				for cscmd in substituted_commands {
					scmd := cscmd.replace(paramk, paramv)
					new_substituted_commands << scmd
				}
				for sc in new_substituted_commands {
					substituted_commands << sc
				}
			}
		}
		for sc in substituted_commands {
			all_commands << sc
		}
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
					flushed_print('${context.cgoback}warming up run: ${i:4}/${context.warmup:-4} for ${cmd:-50s} took ${duration:6} ms ...')
					mut sw := time.new_stopwatch()
					res := os.execute(cmd)
					if res.exit_code != 0 {
						continue
					}
					duration = int(sw.elapsed().milliseconds())
				}
				run_warmups++
			}
			context.clear_line()
			for i in 1 .. (context.count + 1) {
				avg := f64(sum) / f64(i)
				flushed_print('${context.cgoback}Average: ${avg:9.3f}ms | run: ${i:4}/${context.count:-4} | took ${duration:6} ms')
				if context.show_output {
					flushed_print(' | result: ${oldres:s}')
				}
				mut sw := time.new_stopwatch()
				res := scripting.exec(cmd) or { continue }
				duration = int(sw.elapsed().milliseconds())
				if res.exit_code != 0 {
					eprintln('${i:10} non 0 exit code for cmd: $cmd')
					continue
				}
				trimed_output := res.output.trim_right('\r\n')
				trimed_normalized := trimed_output.replace('\r\n', '\n')
				lines := trimed_normalized.split('\n')
				for line in lines {
					context.results[icmd].outputs << line
				}
				context.results[icmd].timings << duration
				sum += duration
				runs++
				oldres = res.output.replace('\n', ' ')
			}
			context.results[icmd].cmd = cmd
			context.results[icmd].icmd = icmd
			context.results[icmd].runs += runs
			context.results[icmd].atiming = new_aints(context.results[icmd].timings, context.nmins,
				context.nmaxs)
			context.clear_line()
			flushed_print(context.cgoback)
			mut m := map[string][]int{}
			ioutputs := context.results[icmd].outputs
			for o in ioutputs {
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
				s := new_aints(v, context.nmins, context.nmaxs)
				println('  $k: $s')
				summary[k] = s
			}
			// merge current raw results to the previous ones
			old_oms := context.results[icmd].oms.move()
			mut new_oms := map[string][]int{}
			for k, v in m {
				if old_oms[k].len == 0 {
					new_oms[k] = v
				} else {
					new_oms[k] << old_oms[k]
					new_oms[k] << v
				}
			}
			context.results[icmd].oms = new_oms.move()
			// println('')
		}
	}
	// create full summaries, taking account of all runs
	for icmd in 0 .. context.results.len {
		mut new_full_summary := map[string]Aints{}
		for k, v in context.results[icmd].oms {
			new_full_summary[k] = new_aints(v, context.nmins, context.nmaxs)
		}
		context.results[icmd].summary = new_full_summary.move()
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
	mut first_marker := ''
	for i, r in context.results {
		first_marker = ' '
		cpercent := (r.atiming.average / base) * 100 - 100
		if r.icmd == 0 {
			first_marker = bold('>')
			first_cmd_percentage = cpercent
		}
		println(' $first_marker${(i + 1):3} | ${cpercent:5.1f}% slower | ${r.cmd:-57s} | $r.atiming')
	}
	$if debugcontext ? {
		println('context: $context')
	}
	if int(base) > context.fail_on_maxtime {
		flushed_print(performance_regression_label)
		println('average time: ${base:6.1f} ms > $context.fail_on_maxtime ms threshold.')
		exit(2)
	}
	if context.fail_on_regress_percent == max_fail_percent || context.results.len < 2 {
		return
	}
	fail_threshold_max := f64(context.fail_on_regress_percent)
	if first_cmd_percentage > fail_threshold_max {
		flushed_print(performance_regression_label)
		println('${first_cmd_percentage:5.1f}% > ${fail_threshold_max:5.1f}% threshold.')
		exit(3)
	}
}
