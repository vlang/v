// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import os.cmdline
import rand
import term
import vhelp
import v.pref

const (
	too_long_line_length = 100
	term_colors          = term.can_show_color_on_stderr()
	is_all               = '-all' in os.args
	hide_warnings        = '-hide-warnings' in os.args
	non_option_args      = cmdline.only_non_options(os.args[2..])
)

struct CheckResult {
pub mut:
	warnings int
	errors   int
	oks      int
}

fn (v1 CheckResult) + (v2 CheckResult) CheckResult {
	return CheckResult{
		warnings: v1.warnings + v2.warnings
		errors: v1.errors + v2.errors
		oks: v1.oks + v2.oks
	}
}

fn main() {
	if non_option_args.len == 0 || '-help' in os.args {
		vhelp.show_topic('check-md')
		exit(0)
	}
	if is_all {
		println('´-all´ flag is deprecated. Please use ´v check-md .´ instead.')
		exit(1)
	}
	mut files_paths := non_option_args.clone()
	mut res := CheckResult{}
	if term_colors {
		os.setenv('VCOLORS', 'always', true)
	}
	for i := 0; i < files_paths.len; i++ {
		file_path := files_paths[i]
		if os.is_dir(file_path) {
			files_paths << md_file_paths(file_path)
			continue
		}
		real_path := os.real_path(file_path)
		lines := os.read_lines(real_path) or {
			println('"$file_path" does not exist')
			res.warnings++
			continue
		}
		mut mdfile := MDFile{
			path: file_path
			lines: lines
		}
		res += mdfile.check()
	}
	if res.warnings > 0 || res.errors > 0 || res.oks > 0 {
		println('\nWarnings: $res.warnings | Errors: $res.errors | OKs: $res.oks')
	}
	if res.errors > 0 {
		exit(1)
	}
}

fn md_file_paths(dir string) []string {
	mut files_to_check := []string{}
	md_files := os.walk_ext(dir, '.md')
	for file in md_files {
		if file.contains_any_substr(['/thirdparty/', 'CHANGELOG']) {
			continue
		}
		files_to_check << file
	}
	return files_to_check
}

fn wprintln(s string) {
	if !hide_warnings {
		println(s)
	}
}

fn ftext(s string, cb fn (string) string) string {
	if term_colors {
		return cb(s)
	}
	return s
}

fn btext(s string) string {
	return ftext(s, term.bold)
}

fn mtext(s string) string {
	return ftext(s, term.magenta)
}

fn rtext(s string) string {
	return ftext(s, term.red)
}

fn wline(file_path string, lnumber int, column int, message string) string {
	return btext('$file_path:${lnumber + 1}:${column + 1}:') + btext(mtext(' warn:')) +
		rtext(' $message')
}

fn eline(file_path string, lnumber int, column int, message string) string {
	return btext('$file_path:${lnumber + 1}:${column + 1}:') + btext(rtext(' error: $message'))
}

const (
	default_command = 'compile'
)

struct VCodeExample {
mut:
	text    []string
	command string
	sline   int
	eline   int
}

enum MDFileParserState {
	markdown
	vexample
	codeblock
}

struct MDFile {
	path  string
	lines []string
mut:
	examples []VCodeExample
	current  VCodeExample
	state    MDFileParserState = .markdown
}

fn (mut f MDFile) check() CheckResult {
	mut res := CheckResult{}
	for j, line in f.lines {
		if line.len > too_long_line_length {
			if f.state == .vexample {
				wprintln(wline(f.path, j, line.len, 'long V example line'))
				wprintln(line)
				res.warnings++
			} else if f.state == .codeblock {
				wprintln(wline(f.path, j, line.len, 'long code block line'))
				wprintln(line)
				res.warnings++
			} else if line.starts_with('|') {
				wprintln(wline(f.path, j, line.len, 'long table'))
				wprintln(line)
				res.warnings++
			} else if line.contains('https') {
				wprintln(wline(f.path, j, line.len, 'long link'))
				wprintln(line)
				res.warnings++
			} else {
				eprintln(eline(f.path, j, line.len, 'line too long'))
				eprintln(line)
				res.errors++
			}
		}
		f.parse_line(j, line)
	}
	res += f.check_examples()
	return res
}

fn (mut f MDFile) parse_line(lnumber int, line string) {
	if line.starts_with('```v') {
		if f.state == .markdown {
			f.state = .vexample
			mut command := line.replace('```v', '').trim_space()
			if command == '' {
				command = default_command
			} else if command == 'nofmt' {
				command += ' $default_command'
			}
			f.current = VCodeExample{
				sline: lnumber
				command: command
			}
		}
		return
	}
	if line.starts_with('```') {
		match f.state {
			.vexample {
				f.state = .markdown
				f.current.eline = lnumber
				f.examples << f.current
				f.current = VCodeExample{}
				return
			}
			.codeblock {
				f.state = .markdown
				return
			}
			.markdown {
				f.state = .codeblock
				return
			}
		}
	}
	if f.state == .vexample {
		f.current.text << line
	}
}

fn (mut f MDFile) debug() {
	for e in f.examples {
		eprintln('f.path: $f.path | example: $e')
	}
}

fn cmdexecute(cmd string) int {
	res := os.execute(cmd)
	if res.exit_code < 0 {
		return 1
	}
	if res.exit_code != 0 {
		eprint(res.output)
	}
	return res.exit_code
}

fn silent_cmdexecute(cmd string) int {
	res := os.execute(cmd)
	return res.exit_code
}

fn get_fmt_exit_code(vfile string, vexe string) int {
	return silent_cmdexecute('"$vexe" fmt -verify $vfile')
}

fn (mut f MDFile) check_examples() CheckResult {
	mut errors := 0
	mut oks := 0
	vexe := pref.vexe_path()
	for e in f.examples {
		if e.command == 'ignore' {
			continue
		}
		if e.command == 'wip' {
			continue
		}
		fname := os.base(f.path).replace('.md', '_md')
		uid := rand.ulid()
		vfile := os.join_path(os.temp_dir(), 'check_${fname}_example_${e.sline}__${e.eline}__${uid}.v')
		mut should_cleanup_vfile := true
		// eprintln('>>> checking example $vfile ...')
		vcontent := e.text.join('\n') + '\n'
		os.write_file(vfile, vcontent) or { panic(err) }
		mut acommands := e.command.split(' ')
		nofmt := 'nofmt' in acommands
		for command in acommands {
			fmt_res := if nofmt { 0 } else { get_fmt_exit_code(vfile, vexe) }
			match command {
				'compile' {
					res := cmdexecute('"$vexe" -w -Wfatal-errors -o x.c $vfile')
					os.rm('x.c') or {}
					if res != 0 || fmt_res != 0 {
						if res != 0 {
							eprintln(eline(f.path, e.sline, 0, 'example failed to compile'))
						}
						if fmt_res != 0 {
							eprintln(eline(f.path, e.sline, 0, 'example is not formatted'))
						}
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'live' {
					res := cmdexecute('"$vexe" -w -Wfatal-errors -live -o x.c $vfile')
					if res != 0 || fmt_res != 0 {
						if res != 0 {
							eprintln(eline(f.path, e.sline, 0, 'example failed to compile with -live'))
						}
						if fmt_res != 0 {
							eprintln(eline(f.path, e.sline, 0, 'example is not formatted'))
						}
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'failcompile' {
					res := silent_cmdexecute('"$vexe" -w -Wfatal-errors -o x.c $vfile')
					os.rm('x.c') or {}
					if res == 0 {
						eprintln(eline(f.path, e.sline, 0, '`failcompile` example compiled'))
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'oksyntax' {
					res := cmdexecute('"$vexe" -w -Wfatal-errors -check-syntax $vfile')
					if res != 0 || fmt_res != 0 {
						if res != 0 {
							eprintln(eline(f.path, e.sline, 0, '`oksyntax` example with invalid syntax'))
						}
						if fmt_res != 0 {
							eprintln(eline(f.path, e.sline, 0, '`oksyntax` example is not formatted'))
						}
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'badsyntax' {
					res := silent_cmdexecute('"$vexe" -w -Wfatal-errors -check-syntax $vfile')
					if res == 0 {
						eprintln(eline(f.path, e.sline, 0, '`badsyntax` example can be parsed fine'))
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'nofmt' {}
				else {
					eprintln(eline(f.path, e.sline, 0, 'unrecognized command: "$command", use one of: wip/ignore/compile/failcompile/oksyntax/badsyntax'))
					should_cleanup_vfile = false
					errors++
				}
			}
		}
		if should_cleanup_vfile {
			os.rm(vfile) or { panic(err) }
		}
	}
	return CheckResult{
		errors: errors
		oks: oks
	}
}
