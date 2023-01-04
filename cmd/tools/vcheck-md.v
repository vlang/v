// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import os.cmdline
import rand
import term
import vhelp
import regex

const (
	too_long_line_length_example   = 120
	too_long_line_length_codeblock = 120
	too_long_line_length_table     = 120
	too_long_line_length_link      = 150
	too_long_line_length_other     = 100
	term_colors                    = term.can_show_color_on_stderr()
	hide_warnings                  = '-hide-warnings' in os.args || '-w' in os.args
	show_progress                  = os.getenv('GITHUB_JOB') == '' && '-silent' !in os.args
	non_option_args                = cmdline.only_non_options(os.args[2..])
	is_verbose                     = os.getenv('VERBOSE') != ''
	vcheckfolder                   = os.join_path(os.vtmp_dir(), 'v', 'vcheck_${os.getuid()}')
	should_autofix                 = os.getenv('VAUTOFIX') != ''
	vexe                           = @VEXE
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
	if '-all' in os.args {
		println('´-all´ flag is deprecated. Please use ´v check-md .´ instead.')
		exit(1)
	}
	if show_progress {
		// this is intended to be replaced by the progress lines
		println('')
	}
	mut files_paths := non_option_args.clone()
	mut res := CheckResult{}
	if term_colors {
		os.setenv('VCOLORS', 'always', true)
	}
	os.mkdir_all(vcheckfolder, mode: 0o700) or {} // keep directory private
	defer {
		os.rmdir_all(vcheckfolder) or {}
	}
	for i := 0; i < files_paths.len; i++ {
		file_path := files_paths[i]
		if os.is_dir(file_path) {
			files_paths << md_file_paths(file_path)
			continue
		}
		real_path := os.real_path(file_path)
		lines := os.read_lines(real_path) or {
			println('"${file_path}" does not exist')
			res.warnings++
			continue
		}
		mut mdfile := MDFile{
			path: file_path
			lines: lines
		}
		res += mdfile.check()
	}
	if res.errors == 0 && show_progress {
		clear_previous_line()
	}
	if res.warnings > 0 || res.errors > 0 || res.oks > 0 {
		println('\nWarnings: ${res.warnings} | Errors: ${res.errors} | OKs: ${res.oks}')
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
	return btext('${file_path}:${lnumber + 1}:${column + 1}:') + btext(mtext(' warn:')) +
		rtext(' ${message}')
}

fn eline(file_path string, lnumber int, column int, message string) string {
	return btext('${file_path}:${lnumber + 1}:${column + 1}:') + btext(rtext(' error: ${message}'))
}

const default_command = 'compile'

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
	path string
mut:
	lines    []string
	examples []VCodeExample
	current  VCodeExample
	state    MDFileParserState = .markdown
}

fn (mut f MDFile) progress(message string) {
	if show_progress {
		clear_previous_line()
		println('File: ${f.path:-30s}, Lines: ${f.lines.len:5}, ${message}')
	}
}

fn (mut f MDFile) check() CheckResult {
	mut res := CheckResult{}
	mut anchor_data := AnchorData{}
	for j, line in f.lines {
		// f.progress('line: $j')
		if f.state == .vexample {
			if line.len > too_long_line_length_example {
				wprintln(wline(f.path, j, line.len, 'example lines must be less than ${too_long_line_length_example} characters'))
				wprintln(line)
				res.warnings++
			}
		} else if f.state == .codeblock {
			if line.len > too_long_line_length_codeblock {
				wprintln(wline(f.path, j, line.len, 'code lines must be less than ${too_long_line_length_codeblock} characters'))
				wprintln(line)
				res.warnings++
			}
		} else if line.starts_with('|') {
			if line.len > too_long_line_length_table {
				wprintln(wline(f.path, j, line.len, 'table lines must be less than ${too_long_line_length_table} characters'))
				wprintln(line)
				res.warnings++
			}
		} else if line.contains('http') {
			if line.all_after('https').len > too_long_line_length_link {
				wprintln(wline(f.path, j, line.len, 'link lines must be less than ${too_long_line_length_link} characters'))
				wprintln(line)
				res.warnings++
			}
		} else if line.len > too_long_line_length_other {
			eprintln(eline(f.path, j, line.len, 'must be less than ${too_long_line_length_other} characters'))
			eprintln(line)
			res.errors++
		}
		if f.state == .markdown {
			anchor_data.add_links(j, line)
			anchor_data.add_link_targets(j, line)
		}

		f.parse_line(j, line)
	}
	anchor_data.check_link_target_match(f.path, mut res)
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
				command += ' ${default_command}'
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

struct Headline {
	line  int
	lable string
	level int
}

struct Anchor {
	line int
}

type AnchorTarget = Anchor | Headline

struct AnchorLink {
	line  int
	lable string
}

struct AnchorData {
mut:
	links   map[string][]AnchorLink
	anchors map[string][]AnchorTarget
}

fn (mut ad AnchorData) add_links(line_number int, line string) {
	query := r'\[(?P<lable>[^\]]+)\]\(\s*#(?P<link>[a-z0-9\-\_\x7f-\uffff]+)\)'
	mut re := regex.regex_opt(query) or { panic(err) }
	res := re.find_all_str(line)

	for elem in res {
		re.match_string(elem)
		link := re.get_group_by_name(elem, 'link')
		ad.links[link] << AnchorLink{
			line: line_number
			lable: re.get_group_by_name(elem, 'lable')
		}
	}
}

fn (mut ad AnchorData) add_link_targets(line_number int, line string) {
	if line.trim_space().starts_with('#') {
		if headline_start_pos := line.index(' ') {
			headline := line.substr(headline_start_pos + 1, line.len)
			link := create_ref_link(headline)
			ad.anchors[link] << Headline{
				line: line_number
				lable: headline
				level: headline_start_pos
			}
		}
	} else {
		query := '<a\\s*id=["\'](?P<link>[a-z0-9\\-\\_\\x7f-\\uffff]+)["\']\\s*/>'
		mut re := regex.regex_opt(query) or { panic(err) }
		res := re.find_all_str(line)

		for elem in res {
			re.match_string(elem)
			link := re.get_group_by_name(elem, 'link')
			ad.anchors[link] << Anchor{
				line: line_number
			}
		}
	}
}

fn (mut ad AnchorData) check_link_target_match(fpath string, mut res CheckResult) {
	mut checked_headlines := []string{}
	mut found_error_warning := false
	for link, linkdata in ad.links {
		if link in ad.anchors {
			checked_headlines << link
			if ad.anchors[link].len > 1 {
				found_error_warning = true
				res.errors++
				for anchordata in ad.anchors[link] {
					eprintln(eline(fpath, anchordata.line, 0, 'multiple link targets of existing link (#${link})'))
				}
			}
		} else {
			found_error_warning = true
			res.errors++
			for brokenlink in linkdata {
				eprintln(eline(fpath, brokenlink.line, 0, 'no link target found for existing link [${brokenlink.lable}](#${link})'))
			}
		}
	}
	for link, anchor_lists in ad.anchors {
		if link !in checked_headlines {
			if anchor_lists.len > 1 {
				for anchor in anchor_lists {
					line := match anchor {
						Headline {
							anchor.line
						}
						Anchor {
							anchor.line
						}
					}
					wprintln(wline(fpath, line, 0, 'multiple link target for non existing link (#${link})'))
					found_error_warning = true
					res.warnings++
				}
			}
		}
	}
	if found_error_warning {
		eprintln('') // fix suppressed last error output
	}
}

// based on a reference sample md doc
// https://github.com/aheissenberger/vlang-markdown-module/blob/master/test.md
fn create_ref_link(s string) string {
	mut result := ''
	for c in s.trim_space() {
		result += match c {
			`a`...`z`, `0`...`9` {
				c.ascii_str()
			}
			`A`...`Z` {
				c.ascii_str().to_lower()
			}
			` `, `-` {
				'-'
			}
			`_` {
				'_'
			}
			else {
				if c > 127 { c.ascii_str() } else { '' }
			}
		}
	}
	return result
}

fn (mut f MDFile) debug() {
	for e in f.examples {
		eprintln('f.path: ${f.path} | example: ${e}')
	}
}

fn cmdexecute(cmd string) int {
	verbose_println(cmd)
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
	verbose_println(cmd)
	res := os.execute(cmd)
	return res.exit_code
}

fn get_fmt_exit_code(vfile string, vexe string) int {
	return silent_cmdexecute('${os.quoted_path(vexe)} fmt -verify ${os.quoted_path(vfile)}')
}

fn (mut f MDFile) check_examples() CheckResult {
	mut errors := 0
	mut oks := 0
	recheck_all_examples: for e in f.examples {
		if e.command == 'ignore' {
			continue
		}
		if e.command == 'wip' {
			continue
		}
		fname := os.base(f.path).replace('.md', '_md')
		uid := rand.ulid()
		cfile := os.join_path(vcheckfolder, '${uid}.c')
		vfile := os.join_path(vcheckfolder, 'check_${fname}_example_${e.sline}__${e.eline}__${uid}.v')
		efile := os.join_path(vcheckfolder, 'check_${fname}_example_${e.sline}__${e.eline}__${uid}.exe')
		mut should_cleanup_vfile := true
		// eprintln('>>> checking example $vfile ...')
		vcontent := e.text.join('\n') + '\n'
		os.write_file(vfile, vcontent) or { panic(err) }
		mut acommands := e.command.split(' ')
		nofmt := 'nofmt' in acommands
		for command in acommands {
			f.progress('example from ${e.sline} to ${e.eline}, command: ${command}')
			fmt_res := if nofmt { 0 } else { get_fmt_exit_code(vfile, vexe) }
			match command {
				'compile' {
					res := cmdexecute('${os.quoted_path(vexe)} -w -Wfatal-errors -o ${os.quoted_path(efile)} ${os.quoted_path(vfile)}')
					if res != 0 || fmt_res != 0 {
						if res != 0 {
							eprintln(eline(f.path, e.sline, 0, 'example failed to compile'))
						}
						f.report_not_formatted_example_if_needed(e, fmt_res, vfile) or {
							unsafe {
								goto recheck_all_examples
							}
						}
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'cgen' {
					res := cmdexecute('${os.quoted_path(vexe)} -w -Wfatal-errors -o ${os.quoted_path(cfile)} ${os.quoted_path(vfile)}')
					if res != 0 || fmt_res != 0 {
						if res != 0 {
							eprintln(eline(f.path, e.sline, 0, 'example failed to generate C code'))
						}
						f.report_not_formatted_example_if_needed(e, fmt_res, vfile) or {
							unsafe {
								goto recheck_all_examples
							}
						}
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'globals' {
					res := cmdexecute('${os.quoted_path(vexe)} -w -Wfatal-errors -enable-globals -o ${os.quoted_path(cfile)} ${os.quoted_path(vfile)}')
					if res != 0 || fmt_res != 0 {
						if res != 0 {
							eprintln(eline(f.path, e.sline, 0, '`example failed to compile with -enable-globals'))
						}
						f.report_not_formatted_example_if_needed(e, fmt_res, vfile) or {
							unsafe {
								goto recheck_all_examples
							}
						}
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'live' {
					res := cmdexecute('${os.quoted_path(vexe)} -w -Wfatal-errors -live -o ${os.quoted_path(cfile)} ${os.quoted_path(vfile)}')
					if res != 0 || fmt_res != 0 {
						if res != 0 {
							eprintln(eline(f.path, e.sline, 0, 'example failed to compile with -live'))
						}
						f.report_not_formatted_example_if_needed(e, fmt_res, vfile) or {
							unsafe {
								goto recheck_all_examples
							}
						}
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'shared' {
					res := cmdexecute('${os.quoted_path(vexe)} -w -Wfatal-errors -shared -o ${os.quoted_path(cfile)} ${os.quoted_path(vfile)}')
					if res != 0 || fmt_res != 0 {
						if res != 0 {
							eprintln(eline(f.path, e.sline, 0, 'module example failed to compile with -shared'))
						}
						f.report_not_formatted_example_if_needed(e, fmt_res, vfile) or {
							unsafe {
								goto recheck_all_examples
							}
						}
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'failcompile' {
					res := silent_cmdexecute('${os.quoted_path(vexe)} -w -Wfatal-errors -o ${os.quoted_path(cfile)} ${os.quoted_path(vfile)}')
					if res == 0 || fmt_res != 0 {
						if res == 0 {
							eprintln(eline(f.path, e.sline, 0, '`failcompile` example compiled'))
						}
						f.report_not_formatted_example_if_needed(e, fmt_res, vfile) or {
							unsafe {
								goto recheck_all_examples
							}
						}
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'oksyntax' {
					res := cmdexecute('${os.quoted_path(vexe)} -w -Wfatal-errors -check-syntax ${os.quoted_path(vfile)}')
					if res != 0 || fmt_res != 0 {
						if res != 0 {
							eprintln(eline(f.path, e.sline, 0, '`oksyntax` example with invalid syntax'))
						}
						f.report_not_formatted_example_if_needed(e, fmt_res, vfile) or {
							unsafe {
								goto recheck_all_examples
							}
						}
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'okfmt' {
					if fmt_res != 0 {
						f.report_not_formatted_example_if_needed(e, fmt_res, vfile) or {
							unsafe {
								goto recheck_all_examples
							}
						}
						eprintln(vcontent)
						should_cleanup_vfile = false
						errors++
						continue
					}
					oks++
				}
				'badsyntax' {
					res := silent_cmdexecute('${os.quoted_path(vexe)} -w -Wfatal-errors -check-syntax ${os.quoted_path(vfile)}')
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
					eprintln(eline(f.path, e.sline, 0, 'unrecognized command: "${command}", use one of: wip/ignore/compile/failcompile/okfmt/nofmt/oksyntax/badsyntax/cgen/globals/live/shared'))
					should_cleanup_vfile = false
					errors++
				}
			}
		}
		os.rm(cfile) or {}
		os.rm(efile) or {}
		if should_cleanup_vfile {
			os.rm(vfile) or { panic(err) }
		}
	}
	return CheckResult{
		errors: errors
		oks: oks
	}
}

fn verbose_println(message string) {
	if is_verbose {
		println(message)
	}
}

fn clear_previous_line() {
	if is_verbose {
		return
	}
	term.clear_previous_line()
}

fn (mut f MDFile) report_not_formatted_example_if_needed(e VCodeExample, fmt_res int, vfile string) ! {
	if fmt_res == 0 {
		return
	}
	eprintln(eline(f.path, e.sline, 0, 'example is not formatted'))
	if !should_autofix {
		return
	}
	f.autofix_example(e, vfile) or {
		if err is ExampleWasRewritten {
			eprintln('>> f.path: ${f.path} | example from ${e.sline} to ${e.eline} was re-formated by vfmt')
			return err
		}
		eprintln('>> f.path: ${f.path} | encountered error while autofixing the example: ${err}')
	}
}

struct ExampleWasRewritten {
	Error
}

fn (mut f MDFile) autofix_example(e VCodeExample, vfile string) ! {
	eprintln('>>> AUTOFIXING f.path: ${f.path} | e.sline: ${e.sline} | vfile: ${vfile}')
	res := cmdexecute('${os.quoted_path(vexe)} fmt -w ${os.quoted_path(vfile)}')
	if res != 0 {
		return error('could not autoformat the example')
	}
	formatted_content_lines := os.read_lines(vfile) or { return }
	mut new_lines := []string{}
	new_lines << f.lines#[0..e.sline + 1]
	new_lines << formatted_content_lines
	new_lines << f.lines#[e.eline..]
	f.update_examples(new_lines)!
	os.rm(vfile) or {}
	f.examples = f.examples.filter(it.sline >= e.sline)
	return ExampleWasRewritten{}
}

fn (mut f MDFile) update_examples(new_lines []string) ! {
	os.write_file(f.path, new_lines.join('\n'))!
	f.lines = new_lines
	f.examples = []
	f.current = VCodeExample{}
	f.state = .markdown
	for j, line in f.lines {
		f.parse_line(j, line)
	}
}
