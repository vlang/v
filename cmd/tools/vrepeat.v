module main

import os
import flag
import time
import math
import term

const tred = term.red
const tbold = term.bold
const tgray = term.gray
const tcyan = term.cyan
const tgreen = term.green
const tbcyan = term.bright_cyan
const tbblue = term.bright_blue
const tdivider = term.h_divider

fn c(cfn fn (string) string, s string) string {
	return term.colorize(cfn, s)
}

const hline = '-'.repeat(80)

fn show_failure_output(output string) {
	eprintln(hline)
	eprint(output)
	eprintln(hline)
}

const max_fail_percent = 100 * 1000
const max_time = 60 * 1000 // ms

const performance_regression_label = 'Performance regression detected, failing since '

struct Aints {
	values []i64 // uS
mut:
	imin    i64 // uS
	imax    i64 // uS
	average f64
	stddev  f64
	nmins   int // number of discarded fastest results
	nmaxs   int // number of discarded slowest results
}

struct CmdResult {
mut:
	runs    int
	cmd     string
	icmd    int
	outputs []string
	ous     map[string][]i64 // uS
	summary map[string]Aints
	timings []i64
	atiming Aints
}

struct Context {
mut:
	run_count               int
	repeats_count           int
	current_run             int
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
	nmins                   int  // number of minimums to discard
	nmaxs                   int  // number of maximums to discard
	ignore_failed           bool // ignore commands that exit with != 0 exit code
	no_vexe_setenv          bool // do not change the VEXE variable

	fail_count   map[string]int // how many times a command has failed so far. Only the first failure output is shown.
	repeat_timer time.StopWatch = time.new_stopwatch()
}

fn new_aints(ovals []i64, extreme_mins int, extreme_maxs int) Aints {
	mut res := Aints{
		values: ovals // remember the original values
		nmins:  extreme_mins
		nmaxs:  extreme_maxs
	}
	// discard the extremes, if needed:
	mut vals := []i64{}
	for x in ovals {
		vals << x
	}
	vals.sort()
	if extreme_mins > 0 {
		vals = vals#[extreme_mins..].clone()
	}
	if extreme_maxs > 0 {
		vals = vals#[..-extreme_maxs].clone()
	}
	// statistical processing of the remaining values:
	mut sum := i64(0)
	mut imin := i64(max_i64)
	mut imax := i64(-max_i64)
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
	return c(tgreen, c(tbold, s))
}

fn (a Aints) str() string {
	avg := bold('${a.average / 1000:6.1f}ms')
	tdev := c(tred, '${a.stddev / 1000:6.1f}ms')
	baseline := '${avg} ± σ: ${tdev},'
	tmin := c(tbcyan, '${f64(a.imin) / 1000:6.1f}ms')
	tmax := c(tbblue, '${f64(a.imax) / 1000:6.1f}ms')
	return '${baseline:-46s} ${tmin}…${tmax}'
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
		maincmd := context.cmd_template.replace('{T}', cmd)
		mut substituted_commands := []string{}
		substituted_commands << maincmd
		for paramk, paramlist in context.cmd_params {
			for paramv in paramlist {
				mut new_substituted_commands := []string{}
				for cscmd in substituted_commands {
					scmd := cscmd.replace('{${paramk}}', paramv)
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
	for si in 0 .. context.series {
		for icmd, cmd in context.commands {
			mut runs := 0
			mut sum := i64(0)
			mut avg := f64(0)
			mut duration := i64(0)
			mut oldres := ''
			series_label := '${icmd + 1}/${context.commands.len}, ${si + 1}/${context.series}'
			line_prefix := '${context.cgoback}Command: ${c(tgray, cmd)}, ${series_label:9}'
			if context.series != 1 || context.commands.len != 1 {
				flushed_print(line_prefix)
			}
			if context.warmup > 0 {
				for i in 0 .. context.warmup {
					flushed_print('${line_prefix}, warm up run: ${i + 1:4}/${context.warmup:-4}, took: ${f64(duration) / 1000:6.1f}ms ...')
					mut sw := time.new_stopwatch()
					res := os.execute(cmd)
					duration = i64(sw.elapsed().microseconds())
					mut should_show_fail_output := false
					if res.exit_code != 0 && !context.ignore_failed {
						if context.fail_count[cmd] == 0 {
							should_show_fail_output = true
						}
						context.fail_count[cmd]++
					}
					if should_show_fail_output {
						eprintln('\nCommand exited with exit code: ${res.exit_code} in ${f64(duration) / 1000:6.1f}ms .')
						eprintln('Use -e or --ignore to ignore the failed commands.')
						eprintln('The failed cmd was: `${c(tred, cmd)}` ; cmd output:')
						show_failure_output(res.output)
					}
				}
			}
			for i in 0 .. context.run_count {
				mut sw := time.new_stopwatch()
				res := os.execute(cmd)
				duration = i64(sw.elapsed().microseconds())
				//
				mut should_show_fail_output := false
				if res.exit_code != 0 && !context.ignore_failed {
					if context.fail_count[cmd] == 0 {
						should_show_fail_output = true
					}
					context.fail_count[cmd]++
				}
				sum += duration
				runs++
				avg = (f64(sum) / f64(i + 1))
				cavg := '${avg / 1000:9.3f}ms'
				flushed_print('${line_prefix}, current average: ${c(tgreen, cavg)}, run ${i + 1:4}/${context.run_count:-4}, took: ${f64(duration) / 1000:6} ms')
				if context.show_output {
					flushed_print(' | result: ${oldres:s}')
				}
				trimmed_output := res.output.trim_right('\r\n')
				trimmed_normalized := trimmed_output.replace('\r\n', '\n')
				lines := trimmed_normalized.split('\n')
				for line in lines {
					context.results[icmd].outputs << line
				}
				context.results[icmd].timings << duration
				oldres = res.output.replace('\n', ' ')
				if should_show_fail_output {
					eprintln('\n${i + 1:10} non 0 exit code for cmd: ${cmd}; cmd output:')
					show_failure_output(res.output)
				}
			}
			context.results[icmd].cmd = cmd
			context.results[icmd].icmd = icmd
			context.results[icmd].runs += runs
			context.results[icmd].atiming = new_aints(context.results[icmd].timings, context.nmins,
				context.nmaxs)
			context.clear_line()
			flushed_print(context.cgoback)
			context.show_timings_details(si, icmd, cmd)
		}
	}
	// create full summaries, taking account of all runs
	for icmd in 0 .. context.results.len {
		mut new_full_summary := map[string]Aints{}
		for k, v in context.results[icmd].ous {
			new_full_summary[k] = new_aints(v, context.nmins, context.nmaxs)
		}
		context.results[icmd].summary = new_full_summary.clone()
	}
}

fn (mut context Context) show_timings_details(si int, icmd int, cmd string) {
	if !cmd.contains('-show-timings') {
		return
	}
	// This is specific to V compilations, when they are run with `-show-timings`,
	// which will show more details about each compilation stage.
	mut m := map[string][]i64{}
	ioutputs := context.results[icmd].outputs
	for o in ioutputs {
		x := o.split('ms ')
		if x.len > 1 {
			v := i64(x[0].trim_left(' ').f64() * 1000)
			k := x[1].all_before(' ')
			m[k] << v
		}
	}
	if m.len == 0 {
		return
	}
	println('> Timing details for series ${si + 1}, icmd: ${icmd + 1}, cmd: `${cmd}`:')
	mut summary := map[string]Aints{}
	for k, v in m {
		// show a temporary summary for the current series/cmd cycle
		s := new_aints(v, context.nmins, context.nmaxs)
		println('  ${k:-40s}: ${s}')
		summary[k] = s
	}
	// merge current raw results to the previous ones
	old_ous := context.results[icmd].ous.clone()
	mut new_ous := map[string][]i64{}
	for k, v in m {
		if old_ous[k].len == 0 {
			new_ous[k] = v
		} else {
			new_ous[k] << old_ous[k]
			new_ous[k] << v
		}
	}
	context.results[icmd].ous = new_ous.clone()
}

fn compare_by_average(a &CmdResult, b &CmdResult) int {
	if a.atiming.average < b.atiming.average {
		return -1
	}
	if a.atiming.average > b.atiming.average {
		return 1
	}
	return 0
}

fn (mut context Context) show_diff_summary() {
	if context.results.len == 0 {
		eprintln('no results')
		exit(5)
	}
	base := context.results[0].atiming.average
	context.results.sort_with_compare(compare_by_average)
	mut first_cmd_percentage := f64(100.0)
	mut first_marker := ''
	if context.results.len == 1 {
		gcmd := c(tgreen, context.results[0].cmd)
		context.show_summary_title('${context.results[0].atiming}, ${context.series} series, ${context.run_count} runs for ${gcmd}')
	} else {
		context.show_summary_title('Summary after ${context.series} series x ${context.run_count} runs (%s are relative to first command, or `base`)')
		for i, r in context.results {
			first_marker = ' '
			cpercent := (r.atiming.average / base) * 100 - 100
			if r.icmd == 0 {
				first_marker = bold('>')
				first_cmd_percentage = cpercent
			}
			mut comparison := ''
			if r.atiming.average != base {
				comparison = '${cpercent:+8.1f}%'
			}
			mut tcomparison := '  base          '
			if r.atiming.average != base {
				tcomparison = readable_comparison(r.atiming.average, base, cpercent)
			}
			gcmd := c(tgreen, r.cmd)
			println(' ${first_marker}${(i + 1):3} ${comparison:7} ${tcomparison:5} ${r.atiming} `${gcmd}`')
		}
	}
	$if debugcontext ? {
		println('context: ${context}')
	}
	if base > f64(context.fail_on_maxtime * 1000) {
		flushed_print(performance_regression_label)
		println('average time: ${base / 1000:6.1f} ms > ${context.fail_on_maxtime} ms threshold.')
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

fn readable_comparison(tcurrent f64, tbase f64, cpercent f64) string {
	is_same := math.abs(cpercent) <= 0.15
	mut label := '~same~'
	if tcurrent < tbase {
		if !is_same {
			label = c(tgreen, 'faster')
		}
		return '${tbase / tcurrent:6.2f}x ${label}'
	} else {
		if !is_same {
			label = c(tcyan, 'slower')
		}
		return '${tcurrent / tbase:6.2f}x ${label}'
	}
}

fn (mut context Context) show_summary_title(line string) {
	mut msg := [line]
	if context.nmins > 0 {
		msg << 'discard mins: ${context.nmins:2}'
	}
	if context.nmaxs > 0 {
		msg << 'discard maxs: ${context.nmaxs:2}'
	}
	if context.repeats_count > 1 {
		msg << 'repeat: ${context.current_run}/${context.repeats_count}, took: ${f64(context.repeat_timer.elapsed().microseconds()) / 1000:8.3f} ms'
		context.repeat_timer.restart()
	}
	println(msg.join(', '))
}

fn (mut context Context) parse_options() ! {
	mut fp := flag.new_flag_parser(os.args#[1..])
	fp.application(os.file_name(os.executable()))
	fp.version('0.0.3')
	fp.description('Repeat command(s) and collect statistics.\nNote: quote each command (argument), when it contains spaces.')
	fp.arguments_description('CMD1 CMD2 ...')
	fp.skip_executable()
	fp.limit_free_args_to_at_least(1)!
	context.show_help = fp.bool('help', `h`, false, 'Show this help screen.')
	context.run_count = fp.int('runs', `r`, 10, 'Run count. Default: 10')
	context.repeats_count = fp.int('repeats', `R`, 1, 'Repeats count (it repeats everything, including reporting). Default: 1')
	context.warmup = fp.int('warmup', `w`, 2, 'Warmup run count. These are done *at the start* of each series, and the timings are ignored. Default: 2')
	context.series = fp.int('series', `s`, 1, 'Series count. `-s 2 -r 4 a b` => aaaabbbbaaaabbbb, while `-s 3 -r 2 a b` => aabbaabbaabb. Default: 1')
	context.ignore_failed = fp.bool('ignore', `e`, false, 'Ignore failed commands (returning a non 0 exit code).')
	context.no_vexe_setenv = fp.bool('no_vexe_reset', `N`, false, 'Do not reset the VEXE env variable at the start. \n                            By default, VEXE will be set to "", to allow for measuring different V executables. Use this option to override it')
	context.use_newline = fp.bool('newline', `n`, false, 'Use \\n, do not overwrite the last line. Produces more output, but easier to diagnose.')
	context.show_output = fp.bool('output', `O`, false, 'Show command stdout/stderr in the progress indicator for each command. Note: slower, for verbose commands.')
	context.verbose = fp.bool('verbose', `v`, false, 'Be more verbose.')
	context.fail_on_maxtime = fp.int('max_time', `m`, max_time, 'Fail with exit code 2, when first cmd takes above M milliseconds (regression). Default: ${max_time}')
	context.fail_on_regress_percent = fp.int('fail_percent', `f`, max_fail_percent, 'Fail with exit code 3, when first cmd is X% slower than the rest (regression). Default: ${max_fail_percent}')
	context.cmd_template = fp.string('template', `t`, '{T}', 'Command template. {T} will be substituted with the current command. Default: {T}. \n                            Here is an example, that will produce and run 24 permutations = (3 for opt) x (2 for source) x (4 = v names):\n                               v repeat -p opt=-check-syntax,-check,"-o x.c" -p source=examples/hello_world.v,examples/hanoi.v --template "./{T} {opt} {source}" vold vnew vold_prod vnew_prod')
	cmd_params := fp.string_multi('parameter', `p`, 'A parameter substitution list. `pp=val1,val2,val2` means that {pp} in the template, will be substituted with *each* of val1, val2, val3.')
	context.nmins = fp.int('nmins', `i`, 0, 'Ignore the BOTTOM X results (minimum execution time). Makes the results more robust to performance flukes. Default: 0')
	context.nmaxs = fp.int('nmaxs', `a`, 0, 'Ignore the TOP X results (maximum execution time). Makes the results more robust to performance flukes. Default: 0')
	for p in cmd_params {
		parts := p.split('=')
		if parts.len > 1 {
			context.cmd_params[parts[0].trim('{}')] = parts[1].split(',')
		}
	}
	if context.show_help {
		println(fp.usage())
		exit(0)
	}
	commands := fp.finalize() or {
		eprintln('Error: ${err}')
		exit(1)
	}
	context.commands = context.expand_all_commands(commands)
	context.reset_results()
	if context.nmins >= context.run_count {
		context.error_no_run_counts_to_report('${context.run_count - 1} bottom results with `-i ${context.nmins}`')
	} else if context.nmaxs >= context.run_count {
		context.error_no_run_counts_to_report('${context.run_count - 1} top results with `-a ${context.nmaxs}`')
	} else if context.nmaxs + context.nmins >= context.run_count {
		context.error_no_run_counts_to_report('${context.nmaxs + context.nmins - 1} results with `-i ${context.nmins}` and `-a ${context.nmaxs}`')
	}
	if context.use_newline {
		context.cline = '\n'
		context.cgoback = '\n'
	} else {
		context.cline = '\r' + tdivider('') + '\r'
		context.cgoback = '\r'
	}
}

fn (mut context Context) error_no_run_counts_to_report(detail string) {
	eprintln('${bold('Warning:')} discarding more than ${detail}, with only `-r ${context.run_count}` runs, will leave you with no results to report at all.')
}

fn (mut context Context) reset_results() {
	context.results = []CmdResult{len: context.commands.len, cap: 20, init: CmdResult{
		outputs: []string{cap: 500}
		timings: []i64{cap: 500}
	}}
}

fn main() {
	mut context := Context{}
	context.parse_options()!
	if !context.no_vexe_setenv {
		// Make sure that we can measure various V executables
		// without influencing them, by presetting VEXE
		os.setenv('VEXE', '', true)
	}
	for i := 1; i <= context.repeats_count; i++ {
		context.current_run = i
		context.reset_results()
		context.run()
		context.show_diff_summary()
	}
}
