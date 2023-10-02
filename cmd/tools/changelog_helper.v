module main

import os

const git_log_cmd = 'git log -n 500 --pretty=format:"%s" --simplify-merges'

enum Category {
	checker
	breaking
	improvements
	parser
	stdlib
	web
	orm
	db
	native
	cgen
	comptime
	tools
	compiler_internals
	examples
}

struct Line {
	category Category
	text     string
}

const log_txt = 'log.txt'

struct App {
	total_lines int
mut:
	counter int
}

fn main() {
	if !os.exists(log_txt) {
		os.execute(git_log_cmd + ' > ' + log_txt)
	}
	lines := os.read_lines(log_txt)!
	changelog_txt := os.read_file('CHANGELOG.md')!.to_lower()
	mut app := &App{
		total_lines: lines.len
	}
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

		app.process_line(line)!
	}
	println('done.')
}

fn (mut app App) process_line(text string) ! {
	if text == '' {
		return
	}
	semicolon_pos := text.index(': ') or {
		println('no `:` in commit, skipping: "${text}"')
		return
	}
	prefix := text[..semicolon_pos]
	// Get category based on keywords in the commit message/prefix
	mut category := Category.improvements
	if text.contains('checker:') {
		category = .checker
	} else if text.contains('cgen:') {
		category = .cgen
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
	} else if is_native(text) {
		category = .native
	} else if is_examples(text) {
		// TODO maybe always skip these as well?
		category = .examples
	} else if text.contains('docs:') || text.contains('doc:') {
		// Always skip docs
		delete_processed_line_from_log(text)!
		return
	}
	//
	else {
		return
	}

	// Trim everything to the left of `:` for some commits (e.g. `checker: `)
	mut s := text
	// println("PREFIX='${prefix}'")
	// if true {
	// exit(0)
	//}
	if semicolon_pos < 15 && prefix in ['checker', 'cgen'] {
		s = '- ' + text[semicolon_pos + 2..].capitalize()
	}

	// Get input from the user
	print('\033[H\033[J')
	println('${app.counter} / ${app.total_lines}')
	// println('\n')
	println(text)
	input := os.input('${category} ?')
	println("INPUT='${input}'")
	match input {
		'' {
			println('GOT ENTER')
			line := Line{category, s}
			save_line(line)!
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
						save_line(line)!
					}
					break
				}
			}
		}
		else {}
	}
	app.counter++
	// Don't forget to remove the line we just processed from log.txt
	delete_processed_line_from_log(text)!
}

fn save_line(line Line) ! {
	println('save line ${line}')
	mut txt := os.read_file('CHANGELOG.md')!

	// match line.category {
	//.checker {
	txt = line.write_at_category(txt) or { return error('') }
	// println(txt.limit(1000))
	//}
	// else {}
	//}
	os.write_file('CHANGELOG.md', txt)!
}

const category_map = {
	Category.checker:    '#### Checker improvements'
	.breaking:           '#### Breaking changes'
	.improvements:       '#### Improvements in the'
	.parser:             '#### Parser improvements'
	.stdlib:             '#### Standard library'
	.web:                '#### Web'
	.orm:                '#### ORM'
	.db:                 '#### Database drivers'
	.native:             '#### Native backend'
	.cgen:               '#### C backend'
	.comptime:           '#### Comptime'
	.tools:              '#### Tools'
	.compiler_internals: '#### Compiler internals'
	.examples:           '#### Examples'
}

fn (l Line) write_at_category(txt string) ?string {
	title := category_map[l.category]
	title_pos := txt.index(title)?
	// Find the position of the ### category title
	pos := txt.index_after('\n', title_pos + 1)
	first_half := txt[..pos]
	second_half := txt[pos..]
	if txt.contains(l.text) {
		// Avoid duplicates (just in case)
		println("Got a duplicate: '${txt}'")
		return txt
	}
	// Now insert the line in the middle, under the ### category title
	mut line_text := l.text

	// Trim "prefix:" for some categories
	// mut capitalized := false
	mut has_prefix := true
	if l.category in [.cgen, .checker, .improvements] {
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
	text := os.read_file(log_txt)!
	new_text := text.replace_once(line, '')
	os.write_file(log_txt, new_text)!
}

const db_strings = [
	'db:',
	'db.sqlite',
	'db.mysql',
	'db.pg',
]

const improvements_strings = [
	'vfmt:',
	'fmt:',
	'all:',
	'v:',
]

const parser_strings = [
	'parser:',
	'ast:',
]

const stdlib_strings = [
	'gg:',
	'json:',
	'time:',
	'sync:',
	'datatypes:',
	'math:',
	'math.big',
	'crypto',
	'sokol:',
	'os:',
	'rand:',
	'math:',
	'toml:',
	'vlib:',
	'arrays:',
	'math.',
	'os.',
	'term:',
	'sync.',
	'builtin:',
	'strconv',
	'readline',
	'cli:',
	'eventbus:',
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

const internal_strings = [
	'scanner:',
	'transformer:',
	'builder:',
	'pref:',
	'v.util',
]

fn is_internal(text string) bool {
	return is_xxx(text, internal_strings)
}

fn is_improvements(text string) bool {
	return is_xxx(text, improvements_strings)
}

const examples_strings = [
	'example',
	'tests',
	'readme:',
	'.md:',
]

fn is_examples(text string) bool {
	return is_xxx(text, examples_strings)
}

const tools_strings = [
	'tools:',
	'vpm:',
	'ci',
	'github:',
	'gitignore',
	'benchmark',
	'v.help:',
]

fn is_tools(text string) bool {
	return is_xxx(text, tools_strings)
}

fn is_parser(text string) bool {
	return is_xxx(text, parser_strings)
}

const web_strings = [
	'vweb:',
	'websocket:',
	'picoev:',
	'mbedtls',
	'net:',
	'net.',
	'wasm:',
	'http:',
]

fn is_web(text string) bool {
	return is_xxx(text, web_strings)
}

const native_strings = [
	'native:',
]

fn is_native(text string) bool {
	return is_xxx(text, native_strings)
}

fn is_xxx(text string, words []string) bool {
	for s in words {
		if text.contains(s) {
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
