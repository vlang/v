module main

import os

const delete_skipped = true

const git_log_cmd = 'git log --pretty=format:"%s" --simplify-merges'

enum Category {
	checker
	breaking
	improvements
	parser
	stdlib
	web
	orm
	db
	cgen
	js_backend
	comptime
	tools
	compiler_internals
	examples
	vfmt
	os_support
	interpreter
}

const category_titles = '#### Improvements in the language

#### V interpreter

#### Breaking changes

#### Checker improvements/fixes

#### Parser improvements

#### Compiler internals

#### Standard library

#### Web

#### ORM

#### Database drivers

#### C backend

#### JavaScript backend

#### Comptime

#### vfmt

#### Tools

#### Operating System support

#### Examples
'

struct Line {
	category Category
	text     string
}

const log_txt = 'log.txt'

struct App {
	version     string // e.g. "0.4.5"
	total_lines int
mut:
	result  string // resulting CHANGELOG.md
	counter int
}

const is_interactive = false

fn main() {
	mut version := ''

	if os.args.len == 2 && os.args[1].starts_with('0.') {
		version = os.args[1]
	} else {
		println('Usage: v run tools/changelog_helper.v 0.4.5')
		return
	}
	if !os.exists(log_txt) {
		os.execute(git_log_cmd + ' > ' + log_txt)
		println('log.txt generated')
		// println('log.txt generated, remove unnecessary commits from it and run the tool again')
		// return
	}
	mut lines := os.read_lines(log_txt)!
	// Trim everything before current version, commit "(tag: 0.4.4) V 0.4.4"
	mut prev_version := get_prev_version(version)
	println('prev version=${prev_version}')
	for i, line in lines {
		if line == ('V ${prev_version}') {
			lines = lines[..i].clone()
			break
		}
	}
	os.write_file(log_txt, lines.join('\n'))!
	mut app := &App{
		total_lines: lines.len
	}
	// Write categories at the top first
	app.result = os.read_file('CHANGELOG.md')!.replace_once('V ${version} TODO', 'V ${version}\n' +
		category_titles)
	os.write_file('CHANGELOG.md', app.result)!
	changelog_txt := os.read_file('CHANGELOG.md')!.to_lower()
	// mut counter := 0 // to display how many commits are left
	for line in lines {
		s := line.trim_space()
		if s == '' {
			app.counter++
		}
	}
	// println('${counter} / ${lines.len}')
	for line in lines {
		s := line.to_lower()
		if line != '' && (changelog_txt.contains(s) || changelog_txt.contains(s.after(':'))) {
			println('Duplicate: "${line}"')
			// skip duplicate
			delete_processed_line_from_log(line)!
			continue
		}

		app.process_line(line.trim_space())!
	}
	println('writing changelog.md')
	if !is_interactive {
		os.write_file('CHANGELOG.md', app.result)!
	}
	println('done.')
}

fn (mut app App) process_line(text string) ! {
	if text == '' {
		return
	}
	// Get category based on keywords in the commit message/prefix
	mut category := Category.examples
	if is_checker(text) {
		category = .checker
	} else if is_interpreter(text) {
		category = .interpreter
	} else if is_skip(text) {
		// Always skip cleanups, typos etc
		println('Skipping line (cleanup/typo)\n${text}\n')
		if delete_skipped {
			delete_processed_line_from_log(text)!
		}
		return
	} else if is_examples(text) {
		category = .examples
	} else if is_os_support(text) {
		category = .os_support
	} else if is_cgen(text) {
		category = .cgen
	} else if is_js_backend(text) {
		category = .js_backend
	} else if is_comptime(text) {
		category = .comptime
	} else if is_db(text) {
		category = .db
	} else if is_stdlib(text) {
		category = .stdlib
	} else if is_orm(text) {
		category = .orm
	} else if is_web(text) {
		category = .web
	} else if is_tools(text) {
		category = .tools
	} else if is_parser(text) {
		category = .parser
	} else if is_internal(text) {
		category = .compiler_internals
	} else if is_improvements(text) {
		category = .improvements
	} else if is_vfmt(text) {
		category = .vfmt
	} else if text.contains('docs:') || text.contains('doc:') {
		// Always skip docs
		delete_processed_line_from_log(text)!
		return
	} else {
		println('Skipping line (unknown category)\n${text}\n')
		// if delete_skipped {
		// delete_processed_line_from_log(text)!
		//}
		return
	}
	println('process_line: cat=${category} "${text}"')
	semicolon_pos := text.index(': ') or { text.len }
	prefix := text[..semicolon_pos]

	// Trim everything to the left of `:` for some commits (e.g. `checker: `)
	mut s := text
	// println("PREFIX='${prefix}'")
	// if true {
	// exit(0)
	//}
	if (semicolon_pos < 15
		&& prefix in ['checker', 'cgen', 'fix', 'orm', 'parser', 'v.parser', 'native', 'ast', 'jsgen', 'v.gen.js', 'fmt', 'vfmt', 'tools', 'examples', 'eval'])
		|| (semicolon_pos < 30 && prefix.contains(', ')) {
		s = '- ' + text[semicolon_pos + 2..].capitalize()
	}

	if is_interactive {
		// Get input from the user
		print('\033[H\033[J')
		println('${app.counter} / ${app.total_lines}')
		// println('\n')
		println(text)
		input := os.input('${category}? ')
		println("INPUT='${input}'")
		match input {
			'' {
				println('GOT ENTER')
				line := Line{category, s}
				save_line_interactive(line)!
			}
			'n', '0', 'no' {
				// Ignore commit
				println('ignored.')
			}
			's', 'skip' {
				// Skip
				println('skipped.')
				return
			}
			'c', 'change' {
				// Change category
				for {
					print_category_hint()
					custom_category := os.input('${category} ?').int()
					if custom_category == 0 {
						println('wrong category')
					} else {
						unsafe {
							line := Line{Category(custom_category - 1), s}
							save_line_interactive(line)!
						}
						break
					}
				}
			}
			else {}
		}

		app.counter++
	} else {
		line := Line{category, s}
		app.save_line(line)!
	}
	// Don't forget to remove the line we just processed from log.txt
	delete_processed_line_from_log(text)!
}

fn (mut app App) save_line(line Line) ! {
	// println('save line ${line}')
	app.result = line.write_at_category(app.result) or { return error('') }
}

fn save_line_interactive(line Line) ! {
	println('save line interactive ${line}')
	mut txt := os.read_file('CHANGELOG.md')!
	txt = line.write_at_category(txt) or { return error('') }
	os.write_file('CHANGELOG.md', txt)!
}

const category_map = {
	Category.checker:    '#### Checker improvements'
	.breaking:           '#### Breaking changes'
	.improvements:       '#### Improvements in the'
	.interpreter:        '#### V interpreter'
	.parser:             '#### Parser improvements'
	.stdlib:             '#### Standard library'
	.web:                '#### Web'
	.orm:                '#### ORM'
	.db:                 '#### Database drivers'
	.cgen:               '#### C backend'
	.js_backend:         '#### JavaScript backend'
	.comptime:           '#### Comptime'
	.tools:              '#### Tools'
	.compiler_internals: '#### Compiler internals'
	.examples:           '#### Examples'
	.vfmt:               '#### vfmt'
	.os_support:         '#### Operating System'
}

fn (l Line) write_at_category(txt string) ?string {
	title := category_map[l.category]
	title_pos := txt.index(title)?
	// Find the position of the ### category title
	pos := txt.index_after('\n', title_pos + 1) or { return none }
	first_half := txt[..pos]
	second_half := txt[pos..]
	if txt.contains(l.text) {
		// Avoid duplicates (just in case)
		println("Got a duplicate: '${l.text}'")
		return txt
	}
	// Now insert the line in the middle, under the ### category title
	mut line_text := l.text

	// Trim "prefix:" for some categories
	// mut capitalized := false
	mut has_prefix := true
	if l.category in [.cgen, .checker, .improvements, .orm, .interpreter] {
		has_prefix = false
		if semicolon_pos := line_text.index(': ') {
			prefix := line_text[..semicolon_pos]
			println("PREFIX='${prefix}'")
			if semicolon_pos < 15 {
				line_text = line_text[semicolon_pos + 2..].capitalize()
				// capitalized = true
			}
		}
	}
	if !has_prefix {
		line_text = line_text.capitalize()
	}
	if !line_text.starts_with('- ') {
		line_text = '- ' + line_text
	}
	return first_half + '\n' + line_text + second_half
}

fn delete_processed_line_from_log(line string) ! {
	lines := os.read_lines(log_txt)!
	mut new_lines := []string{cap: lines.len}
	mut was_deleted := false
	for existing_line in lines {
		if !was_deleted && existing_line == line {
			was_deleted = true
			continue
		}
		new_lines << existing_line
	}
	os.write_file(log_txt, new_lines.join('\n'))!
}

const db_strings = [
	'db:',
	'db.sqlite',
	'db.mysql',
	'db.pg',
	'db.mssql',
	'db.redis',
	'pg:',
	'mysql:',
]

const parser_strings = [
	'parser:',
	'ast:',
]

const stdlib_strings = [
	'aes-gcm',
	'archive:',
	'closure:',
	'compress',
	'context',
	'dtm2:',
	'encoding:',
	'executor:',
	'goroutines:',
	'gg:',
	'hash.',
	'image:',
	'json:',
	'json2:',
	'time:',
	'sync:',
	'datatypes:',
	'datatypes.',
	'math:',
	'math.',
	'math.big',
	'crypto',
	'sokol',
	'os:',
	'rand:',
	'rand.',
	'math:',
	'toml:',
	'vlib:',
	'arrays:',
	'os.',
	'term:',
	'sync.',
	'builtin:',
	'builtin,',
	'builtin.',
	'strconv',
	'readline',
	'cli:',
	'eventbus:',
	'encoding.',
	'bitfield:',
	'io:',
	'io.',
	'log:',
	'lz:',
	'maps:',
	'mcp:',
	'flag:',
	'regex:',
	'regex.',
	'tmpl:',
	'hash:',
	'stbi:',
	'snappy:',
	'strings:',
	'term.ui:',
	'atomic:',
	'context:',
	'thirdparty',
	'x.markdown:',
	'data analysis tool library',
	'yaml:',
]

fn is_stdlib(text string) bool {
	return is_xxx(text, stdlib_strings)
}

fn is_db(text string) bool {
	return is_xxx(text, db_strings)
}

const orm_strings = [
	'orm:',
]

fn is_orm(text string) bool {
	return is_xxx(text, orm_strings)
}

const cgen_strings = [
	'c compilation error',
	'cgen:',
	'cgen,',
	'gen/c:',
	'preserve interface arrays',
	'v.gen.c:',
]

fn is_cgen(text string) bool {
	return is_xxx(text, cgen_strings)
}

const js_backend_strings = [
	'js:',
	'v.gen.js:',
	'jsgen:',
]

fn is_js_backend(text string) bool {
	return is_xxx(text, js_backend_strings)
}

const internal_strings = [
	'ast,pref,genc:',
	'compiler:',
	'gen:',
	'gc:',
	'libgc:',
	'native:',
	'reflection:',
	'scanner:',
	'transformer:',
	'markused:',
	'builder:',
	'pref:',
	'v.util',
	'v.generic',
	'v.comptime',
	'table:',
	'util:',
	'v2:',
	'v2/',
	'v2 ',
	'v3:',
	'v3 ownership',
	'vgc:',
	'vlib,builder,v3:',
]

fn is_internal(text string) bool {
	return is_xxx(text, internal_strings)
}

const improvements_strings = [
	'@[soa]',
	'all:',
	'c function wrappers',
	'v:',
	'coroutines:',
	'autofree',
]

fn is_improvements(text string) bool {
	return is_xxx(text, improvements_strings)
}

const examples_strings = [
	'example',
]
const skip_strings = [
	'tests',
	'test list',
	'test pass',
	'test_all',
	'test failures',
	'test failure',
	'test fixes',
	'test regressions',
	'test ordering',
	'test coverage',
	'test:',
	'library test',
	'readme:',
	'update docs',
	'doc comment',
	'.md:',
	': format',
	': vfmt',
	'quick fmt',
	'all code is formatted',
	'typos',
	' typo',
	'cleanup',
	'clean up',
	'build(deps)',
	'funding',
	'master ci',
	'fix ci',
	'ci regressions',
	'tools ci',
	'benchmark',
	'bench:',
	'remove unused',
	'removed unused',
	'revert:',
	'dynamic_template_manager_test',
	'vtmp_cov',
]

fn is_examples(text string) bool {
	return is_xxx(text, examples_strings)
}

fn is_skip(text string) bool {
	lower_text := text.to_lower()
	return lower_text in ['fixes', 'ok'] || lower_text.starts_with('fix #')
		|| (lower_text.contains('example') && (lower_text.contains('fix')
		|| lower_text.contains('update'))) || is_xxx(text, skip_strings)
}

const tools_strings = [
	'build:',
	'cmd:',
	'cmd/tools/',
	'editors:',
	'help:',
	'make:',
	'make.bat:',
	'makev.bat:',
	'tools:',
	'vpm:',
	'ci:',
	'github:',
	'gitignore',
	'benchmark',
	'v.help:',
	'vtest',
	'repl',
	'REPL',
	'vet',
	'tools.',
	'GNUmakefile',
	'Dockerfile',
	'vcomplete',
	'vwatch',
	'vsh:',
	'changelog',
]

fn is_tools(text string) bool {
	return is_xxx(text, tools_strings)
}

fn is_parser(text string) bool {
	return is_xxx(text, parser_strings)
}

const web_strings = [
	'fasthttp',
	'veb',
	'vweb',
	'websocket:',
	'pico',
	'x.sessions',
	'picoev:',
	'mbedtls',
	'net:',
	'net.',
	'wasm:',
	'http:',
	'xsessions:',
]

fn is_web(text string) bool {
	return is_xxx(text, web_strings)
}

const vfmt_strings = [
	'attribute call syntax in vfmt',
	'vfmt:',
	'fmt:',
]

fn is_vfmt(text string) bool {
	return is_xxx(text, vfmt_strings)
}

const os_support_strings = [
	'FreeBSD',
	'freebsd',
	'OpenBSD',
	'openbsd',
	'macOS',
	'macos',
	'Windows',
	'windows',
	'Linux',
	'linux',
	'ios module',
	'ppc64',
	'raspberry pi',
	'regqueryinfokeyw',
	'sparc64',
	'tcc/bionic',
	'this app cannot run on your pc',
	'msvc:',
]

fn is_os_support(text string) bool {
	return is_xxx(text, os_support_strings)
}

fn is_comptime(text string) bool {
	lower_text := text.to_lower()
	return lower_text.starts_with('comptime:') || lower_text.contains(': comptime ')
		|| lower_text.starts_with('gen: fix comptime')
}

fn is_interpreter(text string) bool {
	lower_text := text.to_lower()
	return lower_text.starts_with('eval:') || lower_text.starts_with('v2.eval:')
		|| lower_text.contains('eval backend')
}

fn is_checker(text string) bool {
	lower_text := text.to_lower()
	return lower_text.contains('checker:') || lower_text.contains('checker(')
		|| lower_text.contains('#26848')
}

fn is_xxx(text string, words []string) bool {
	lower_text := text.to_lower()
	for s in words {
		if lower_text.contains(s.to_lower()) {
			return true
		}
	}
	return false
}

fn print_category_hint() {
	$for val in Category.values {
		println('${int(val.value) + 1} - ${val.name}; ')
	}
}

// For 0.4.12 returns 0.4.11 etc
fn get_prev_version(version string) string {
	parts := version.split('.')
	if parts.len != 3 {
		return ''
	}
	major := parts[0]
	minor := parts[1]
	patch := parts[2].int()
	return '${major}.${minor}.${patch - 1}'
}
