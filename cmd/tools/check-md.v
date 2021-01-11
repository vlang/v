module main

import os
import os.cmdline
import rand
import term
import v.pref

const (
	too_long_line_length = 100
	term_colors          = term.can_show_color_on_stderr()
	is_all               = '-all' in os.args
	hide_warnings        = '-hide-warnings' in os.args
	non_option_args      = cmdline.only_non_options(os.args[1..])
)

fn wprintln(s string) {
	if !hide_warnings {
		println(s)
	}
}

fn main() {
	if os.args.len == 1 {
		println('Usage: checks the passed markdown files for correct ```v ``` code blocks, 
and for other style violations. like too long lines/links etc...
a) `v run cmd/tools/check-md.v -all` - will check *all* .md files in the folders.
b) `v run cmd/tools/check-md.v doc/docs.md` - will only check a single file.
c) `v run cmd/tools/check-md.v -hide-warnings file.md` - same, but will not print warnings, only errors.

NB: There are several special keywords, which you can put after the code fences for v.
These are:
	compile      - default, you do not need to specify it. cmd/tools/check-md.v compile the example.
	ignore       - ignore the example, useful for examples that just use the syntax highlighting
	failcompile  - known failing compilation. Useful for examples demonstrating compiler errors.
	oksyntax     - it should parse, it may not compile. Useful for partial examples.
	badsyntax    - known bad syntax, it should not even parse
	wip          - like ignore; a planned feature; easy to search.
')
		exit(0)
	}
	files_paths := if is_all { md_file_paths() } else { non_option_args }
	mut warnings := 0
	mut errors := 0
	mut oks := 0
	mut all_md_files := []MDFile{}
	if term_colors {
		os.setenv('VCOLORS', 'always', true)
	}
	for file_path in files_paths {
		real_path := os.real_path(file_path)
		lines := os.read_lines(real_path) or {
			println('"$file_path" does not exist')
			warnings++
			continue
		}
		mut mdfile := MDFile{
			path: file_path
		}
		for i, line in lines {
			if line.len > too_long_line_length {
				if mdfile.state == .vexample {
					wprintln(wline(file_path, i, line.len, 'long V example line'))
					warnings++
				} else if mdfile.state == .codeblock {
					wprintln(wline(file_path, i, line.len, 'long code block line'))
					warnings++
				} else if line.starts_with('|') {
					wprintln(wline(file_path, i, line.len, 'long table'))
					warnings++
				} else if line.contains('https') {
					wprintln(wline(file_path, i, line.len, 'long link'))
					warnings++
				} else {
					eprintln(eline(file_path, i, line.len, 'line too long'))
					errors++
				}
			}
			mdfile.parse_line(i, line)
		}
		all_md_files << mdfile
	}
	for mut mdfile in all_md_files {
		new_errors, new_oks := mdfile.check_examples()
		errors += new_errors
		oks += new_oks
	}
	// println('all_md_files: $all_md_files')
	if warnings > 0 || errors > 0 || oks > 0 {
		println('\nWarnings: $warnings | Errors: $errors | OKs: $oks')
	}
	if errors > 0 {
		exit(1)
	}
}

fn md_file_paths() []string {
	mut files_to_check := []string{}
	md_files := os.walk_ext('.', '.md')
	for file in md_files {
		if file.starts_with('./thirdparty') {
			continue
		}
		if file.contains('CHANGELOG') {
			continue
		}
		files_to_check << file
	}
	return files_to_check
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
	return btext('$file_path:${lnumber + 1}:${column + 1}:') + btext(mtext(' warn:')) + rtext(' $message')
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
	path     string
mut:
	examples []VCodeExample
	current  VCodeExample
	state    MDFileParserState = .markdown
}

fn (mut f MDFile) parse_line(lnumber int, line string) {
	if line.starts_with('```v') {
		if f.state == .markdown {
			f.state = .vexample
			mut command := line.replace('```v', '').trim_space()
			if command == '' {
				command = default_command
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

fn (mut f MDFile) dump() {
	for e in f.examples {
		eprintln('f.path: $f.path | example: $e')
	}
}

fn cmdexecute(cmd string) int {
	res := os.exec(cmd) or { return 1 }
	if res.exit_code != 0 {
		eprint(res.output)
	}
	return res.exit_code
}

fn silent_cmdexecute(cmd string) int {
	res := os.exec(cmd) or { return 1 }
	return res.exit_code
}

fn get_fmt_exit_code(vfile string, vexe string) int {
	return silent_cmdexecute('"$vexe" fmt -verify $vfile')
}

fn (mut f MDFile) check_examples() (int, int) {
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
					os.rm('x.c') or { }
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
					os.rm('x.c') or { }
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
	return errors, oks
}
