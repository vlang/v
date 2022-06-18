// Copyright (c) 2020 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import os
import flag

const (
	tool_name        = 'v missdoc'
	tool_version     = '0.1.0'
	tool_description = 'Prints all V functions in .v files under PATH/, that do not yet have documentation comments.'
	work_dir_prefix  = normalise_path(os.real_path(os.wd_at_startup) + os.path_separator)
)

struct UndocumentedFN {
	file      string
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
mut:
	verify          bool
	diff            bool
	additional_args []string
}

fn (opt Options) collect_undocumented_functions_in_dir(directory string) []UndocumentedFN {
	mut files := []string{}
	collect(directory, mut files, fn (npath string, mut accumulated_paths []string) {
		if !npath.ends_with('.v') {
			return
		}
		if npath.ends_with('_test.v') {
			return
		}
		accumulated_paths << npath
	})
	mut undocumented_fns := []UndocumentedFN{}
	for file in files {
		if !opt.js && file.ends_with('.js.v') {
			continue
		}
		if opt.exclude.len > 0 && opt.exclude.any(file.contains(it)) {
			continue
		}
		undocumented_fns << opt.collect_undocumented_functions_in_file(file)
	}
	return undocumented_fns
}

fn (opt &Options) collect_undocumented_functions_in_file(nfile string) []UndocumentedFN {
	file := os.real_path(nfile)
	contents := os.read_file(file) or { panic(err) }
	lines := contents.split('\n')
	mut list := []UndocumentedFN{}
	mut comments := []string{}
	mut tags := []string{}
	for i, line in lines {
		if line.starts_with('//') {
			comments << line
		} else if line.trim_space().starts_with('[') {
			tags << collect_tags(line)
		} else if line.starts_with('pub fn')
			|| (opt.private && (line.starts_with('fn ') && !(line.starts_with('fn C.')
			|| line.starts_with('fn main')))) {
			if comments.len == 0 {
				clean_line := line.all_before_last(' {')
				list << UndocumentedFN{
					line: i + 1
					signature: clean_line
					tags: tags
					file: file
				}
			}
			tags = []
			comments = []
		} else {
			tags = []
			comments = []
		}
	}
	return list
}

fn (opt &Options) collect_undocumented_functions_in_path(path string) []UndocumentedFN {
	mut undocumented_functions := []UndocumentedFN{}
	if os.is_file(path) {
		undocumented_functions << opt.collect_undocumented_functions_in_file(path)
	} else {
		undocumented_functions << opt.collect_undocumented_functions_in_dir(path)
	}
	return undocumented_functions
}

fn (opt &Options) report_undocumented_functions_in_path(path string) int {
	mut list := opt.collect_undocumented_functions_in_path(path)
	opt.report_undocumented_functions(list)
	return list.len
}

fn (opt &Options) report_undocumented_functions(list []UndocumentedFN) {
	if list.len > 0 {
		for undocumented_fn in list {
			mut line_numbers := '$undocumented_fn.line:0:'
			if opt.no_line_numbers {
				line_numbers = ''
			}
			tags_str := if opt.collect_tags && undocumented_fn.tags.len > 0 {
				'$undocumented_fn.tags'
			} else {
				''
			}
			file := undocumented_fn.file
			ofile := if opt.relative_paths {
				file.replace(work_dir_prefix, '')
			} else {
				os.real_path(file)
			}
			if opt.deprecated {
				println('$ofile:$line_numbers$undocumented_fn.signature $tags_str')
			} else {
				mut has_deprecation_tag := false
				for tag in undocumented_fn.tags {
					if tag.starts_with('deprecated') {
						has_deprecation_tag = true
						break
					}
				}
				if !has_deprecation_tag {
					println('$ofile:$line_numbers$undocumented_fn.signature $tags_str')
				}
			}
		}
	}
}

fn (opt &Options) diff_undocumented_functions_in_paths(path_old string, path_new string) []UndocumentedFN {
	old := os.real_path(path_old)
	new := os.real_path(path_new)

	mut old_undocumented_functions := opt.collect_undocumented_functions_in_path(old)
	mut new_undocumented_functions := opt.collect_undocumented_functions_in_path(new)

	mut differs := []UndocumentedFN{}
	if new_undocumented_functions.len > old_undocumented_functions.len {
		for new_undoc_fn in new_undocumented_functions {
			new_relative_file := new_undoc_fn.file.replace(new, '').trim_string_left(os.path_separator)
			mut found := false
			for old_undoc_fn in old_undocumented_functions {
				old_relative_file := old_undoc_fn.file.replace(old, '').trim_string_left(os.path_separator)
				if new_relative_file == old_relative_file
					&& new_undoc_fn.signature == old_undoc_fn.signature {
					found = true
					break
				}
			}
			if !found {
				differs << new_undoc_fn
			}
		}
	}
	differs.sort_with_compare(sort_undoc_fns)
	return differs
}

fn sort_undoc_fns(a &UndocumentedFN, b &UndocumentedFN) int {
	if a.file < b.file {
		return -1
	}
	if a.file > b.file {
		return 1
	}
	// same file sort by signature
	else {
		if a.signature < b.signature {
			return -1
		}
		if a.signature > b.signature {
			return 1
		}
		return 0
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
	mut fp := flag.new_flag_parser(os.args[1..]) // skip the "v" command.
	fp.application(tool_name)
	fp.version(tool_version)
	fp.description(tool_description)
	fp.arguments_description('PATH [PATH]...')
	fp.skip_executable() // skip the "missdoc" command.

	// Collect tool options
	mut opt := Options{
		show_help: fp.bool('help', `h`, false, 'Show this help text.')
		deprecated: fp.bool('deprecated', `d`, false, 'Include deprecated functions in output.')
		private: fp.bool('private', `p`, false, 'Include private functions in output.')
		js: fp.bool('js', 0, false, 'Include JavaScript functions in output.')
		no_line_numbers: fp.bool('no-line-numbers', `n`, false, 'Exclude line numbers in output.')
		collect_tags: fp.bool('tags', `t`, false, 'Also print function tags if any is found.')
		exclude: fp.string_multi('exclude', `e`, '')
		relative_paths: fp.bool('relative-paths', `r`, false, 'Use relative paths in output.')
		diff: fp.bool('diff', 0, false, 'exit(1) and show difference between two PATH inputs, return 0 otherwise.')
		verify: fp.bool('verify', 0, false, 'exit(1) if documentation is missing, 0 otherwise.')
	}

	opt.additional_args = fp.finalize() or { panic(err) }

	if opt.show_help {
		println(fp.usage())
		exit(0)
	}
	if opt.additional_args.len == 0 {
		println(fp.usage())
		eprintln('Error: $tool_name is missing PATH input')
		exit(1)
	}
	// Allow short-long versions to prevent false positive situations, should
	// the user miss a `-`. E.g.: the `-verify` flag would be ignored and missdoc
	// will return 0 for success plus a list of any undocumented functions.
	if '-verify' in opt.additional_args {
		opt.verify = true
	}
	if '-diff' in opt.additional_args {
		opt.diff = true
	}
	if opt.diff {
		if opt.additional_args.len < 2 {
			println(fp.usage())
			eprintln('Error: $tool_name --diff needs two valid PATH inputs')
			exit(1)
		}
		path_old := opt.additional_args[0]
		path_new := opt.additional_args[1]
		if !(os.is_file(path_old) || os.is_dir(path_old)) || !(os.is_file(path_new)
			|| os.is_dir(path_new)) {
			println(fp.usage())
			eprintln('Error: $tool_name --diff needs two valid PATH inputs')
			exit(1)
		}
		list := opt.diff_undocumented_functions_in_paths(path_old, path_new)
		if list.len > 0 {
			opt.report_undocumented_functions(list)
			exit(1)
		}
		exit(0)
	}
	mut total := 0
	for path in opt.additional_args {
		if os.is_file(path) || os.is_dir(path) {
			total += opt.report_undocumented_functions_in_path(path)
		}
	}
	if opt.verify && total > 0 {
		exit(1)
	}
}
