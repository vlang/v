// Copyright (c) 2020 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import flag

const (
	tool_name        = os.file_name(os.executable())
	tool_version     = '0.0.3'
	tool_description = 'Prints all V functions in .v files under PATH/, that do not yet have documentation comments.'
	work_dir_prefix  = normalise_path(os.real_path(os.wd_at_startup) + '/')
)

struct UndocumentedFN {
	line      int
	signature string
	tags      []string
}

struct Options {
	show_help       bool
	collect_tags    bool
	deprecated      bool
	private         bool
	js              bool
	no_line_numbers bool
	exclude         []string
	relative_paths  bool
}

fn (opt Options) report_undocumented_functions_in_path(path string) {
	mut files := []string{}
	collect(path, mut files, fn (npath string, mut accumulated_paths []string) {
		if !npath.ends_with('.v') {
			return
		}
		if npath.ends_with('_test.v') {
			return
		}
		accumulated_paths << npath
	})
	for file in files {
		if !opt.js && file.ends_with('.js.v') {
			continue
		}
		if opt.exclude.len > 0 && opt.exclude.any(file.contains(it)) {
			continue
		}
		opt.report_undocumented_functions_in_file(file)
	}
}

fn (opt &Options) report_undocumented_functions_in_file(nfile string) {
	file := os.real_path(nfile)
	contents := os.read_file(file) or { panic(err) }
	lines := contents.split('\n')
	mut info := []UndocumentedFN{}
	for i, line in lines {
		if line.starts_with('pub fn') || (opt.private && (line.starts_with('fn ')
			&& !(line.starts_with('fn C.') || line.starts_with('fn main')))) {
			// println('Match: $line')
			if i > 0 && lines.len > 0 {
				mut line_above := lines[i - 1]
				if !line_above.starts_with('//') {
					mut tags := []string{}
					mut grab := true
					for j := i - 1; j >= 0; j-- {
						prev_line := lines[j]
						if prev_line.contains('}') { // We've looked back to the above scope, stop here
							break
						} else if prev_line.starts_with('[') {
							tags << collect_tags(prev_line)
							continue
						} else if prev_line.starts_with('//') { // Single-line comment
							grab = false
							break
						}
					}
					if grab {
						clean_line := line.all_before_last(' {')
						info << UndocumentedFN{i + 1, clean_line, tags}
					}
				}
			}
		}
	}
	if info.len > 0 {
		for undocumented_fn in info {
			mut line_numbers := '$undocumented_fn.line:0:'
			if opt.no_line_numbers {
				line_numbers = ''
			}
			tags_str := if opt.collect_tags && undocumented_fn.tags.len > 0 {
				'$undocumented_fn.tags'
			} else {
				''
			}
			ofile := if opt.relative_paths {
				nfile.replace(work_dir_prefix, '')
			} else {
				os.real_path(nfile)
			}
			if opt.deprecated {
				println('$ofile:$line_numbers$undocumented_fn.signature $tags_str')
			} else {
				if 'deprecated' !in undocumented_fn.tags {
					println('$ofile:$line_numbers$undocumented_fn.signature $tags_str')
				}
			}
		}
	}
}

fn normalise_path(path string) string {
	return path.replace('\\', '/')
}

fn collect(path string, mut l []string, f fn (string, mut []string)) {
	if !os.is_dir(path) {
		return
	}
	mut files := os.ls(path) or { return }
	for file in files {
		p := normalise_path(os.join_path_single(path, file))
		if os.is_dir(p) && !os.is_link(p) {
			collect(p, mut l, f)
		} else if os.exists(p) {
			f(p, mut l)
		}
	}
	return
}

fn collect_tags(line string) []string {
	mut cleaned := line.all_before('/')
	cleaned = cleaned.replace_each(['[', '', ']', '', ' ', ''])
	return cleaned.split(',')
}

fn main() {
	if os.args.len == 1 {
		println('Usage: $tool_name PATH \n$tool_description\n$tool_name -h for more help...')
		exit(1)
	}
	mut fp := flag.new_flag_parser(os.args[1..])
	fp.application(tool_name)
	fp.version(tool_version)
	fp.description(tool_description)
	fp.arguments_description('PATH [PATH]...')
	// Collect tool options
	opt := Options{
		show_help: fp.bool('help', `h`, false, 'Show this help text.')
		deprecated: fp.bool('deprecated', `d`, false, 'Include deprecated functions in output.')
		private: fp.bool('private', `p`, false, 'Include private functions in output.')
		js: fp.bool('js', 0, false, 'Include JavaScript functions in output.')
		no_line_numbers: fp.bool('no-line-numbers', `n`, false, 'Exclude line numbers in output.')
		collect_tags: fp.bool('tags', `t`, false, 'Also print function tags if any is found.')
		exclude: fp.string_multi('exclude', `e`, '')
		relative_paths: fp.bool('relative-paths', `r`, false, 'Use relative paths in output.')
	}
	if opt.show_help {
		println(fp.usage())
		exit(0)
	}
	for path in os.args[1..] {
		if os.is_file(path) {
			opt.report_undocumented_functions_in_file(path)
		} else {
			opt.report_undocumented_functions_in_path(path)
		}
	}
}
