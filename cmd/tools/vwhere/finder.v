module main

import os
import term
import regex
import os.cmdline

// Finder is entity that contains all the logic
struct Finder {
mut:
	symbol   Symbol
	visib    Visibility
	mutab    Mutability
	name     string
	modul    string
	receiver string
	dirs     []string
	matches  []Match
}

fn (mut fdr Finder) configure_from_arguments(args []string) {
	match args.len {
		1 {
			fdr.name = args[0]
		}
		else {
			fdr.symbol.set_from_str(args[0])
			if fdr.symbol == .method && !args[1].contains('.') {
				make_and_print_error('method require a special notation:', [
					'Receiver.method',
				], '${args[1]}')
			} else if fdr.symbol == .method {
				temp_args := args[1].split('.')
				fdr.receiver = temp_args[0]
				fdr.name = temp_args[1]
			} else {
				fdr.name = args[1]
			}
			if fdr.name.contains('-') {
				make_and_print_error('It seems you forgot positional arg name:', [], fdr.name)
			}
			fdr.visib.set_from_str(cmdline.option(args, '-vis', '${Visibility.all}'))
			if fdr.symbol == .var && fdr.visib != .all {
				make_and_print_error('-vis $fdr.visib just can be setted with symbol_type:',
					['fn', 'method', 'const', 'struct', 'enum', 'interface', 'regexp'],
					'$fdr.symbol')
			}
			fdr.mutab.set_from_str(cmdline.option(args, '-mut', '${Mutability.any}'))
			if fdr.symbol != .var && fdr.mutab != .any {
				make_and_print_error('-mut $fdr.mutab just can be setted with symbol_type:',
					['var'], '$fdr.symbol')
			}
			fdr.modul = cmdline.option(args, '-mod', '')
			fdr.dirs = cmdline.options(args, '-dir')
		}
	}
}

fn (mut fdr Finder) search_for_matches() {
	// Define where search
	mut recursive := true
	mut paths_to_search := []string{}
	if fdr.dirs.len == 0 && fdr.modul == '' {
		paths_to_search << [current_dir, vmod_dir]
		if vlib_dir !in paths_to_search {
			paths_to_search << vlib_dir
		}
		paths_to_search << vmod_paths
	} else if fdr.dirs.len == 0 && fdr.modul != '' {
		paths_to_search << if fdr.modul == 'main' { current_dir } else { resolve_module(fdr.modul) or {
				panic(err)} }
	} else if fdr.dirs.len != 0 && fdr.modul == '' {
		recursive = false
		paths_to_search << fdr.dirs.map(resolve_module(it) or { panic(err) })
	} else {
		recursive = false
		paths_to_search << if fdr.modul == 'main' { current_dir } else { resolve_module(fdr.modul) or {
				panic(err)} }
		paths_to_search << fdr.dirs.map(resolve_module(it) or { panic(err) })
	}
	// for p in paths_to_search {
	// 	println(p)
	// }
	mut files_to_search := []string{}
	for path in paths_to_search {
		files_to_search << collect_v_files(path, recursive) or { panic(err) }
	}
	// for f in files_to_search {
	// 	println(f)
	// }

	// Auxiliar rgx
	sp := r'\s*'
	op := r'\('
	cp := r'\)'

	// Build regex query
	sy := '$fdr.symbol'
	st := if fdr.receiver != '' { '$sp$op$sp[a-z].*$sp$fdr.receiver$cp$sp' } else { '.*' }
	na := '$fdr.name'

	query := match fdr.symbol {
		.@fn {
			'.*$sy$sp$na$sp${op}.*${cp}.*'
		}
		.method {
			'.*fn$st$na$sp${op}.*${cp}.*'
		}
		.var {
			'.*$na$sp:=.*'
		}
		.@const {
			'.*$na$sp = .*'
		}
		.regexp {
			'$na'
		}
		else {
			'.*$sy$sp$na${sp}.*' // for struct, enum and interface
		}
	}
	// println(query)
	for file in files_to_search {
		fdr.search_within_file(file, query)
	}
}

fn (mut fdr Finder) search_within_file(file string, query string) {
	mut re := regex.regex_opt(query) or { panic(err) }
	lines := os.read_lines(file) or { panic(err) }
	mut const_found := if fdr.symbol == .@const { false } else { true }
	mut n_line := 1
	for line in lines {
		match fdr.visib {
			.all {
				if line.contains('const (') {
					const_found = true
				}
			}
			.@pub {
				if line.contains('pub const (') {
					const_found = true
				}
			}
			.pri {
				if line.contains('const (') && !line.contains('pub') {
					const_found = true
				}
			}
		}
		if re.matches_string(line) && (const_found || line.contains('const')) {
			words := line.split(' ').filter(it != '').map(it.trim('\t'))
			match fdr.visib {
				.all {}
				.@pub {
					if 'pub' !in words && fdr.symbol != .@const {
						continue
					}
				}
				.pri {
					if 'pub' in words && fdr.symbol != .@const {
						continue
					}
				}
			}
			match fdr.mutab {
				.any {}
				.yes {
					if 'mut' !in words {
						continue
					}
				}
				.not {
					if 'mut' in words {
						continue
					}
				}
			}
			fdr.matches << Match{file, n_line, words.join(' ').trim(' {')}
		}
		if line.starts_with(')') && fdr.symbol == .@const {
			const_found = false
		}
		n_line++
	}
}

fn (fdr Finder) show_results() {
	if fdr.matches.len < 1 && (verbose || header) {
		print(fdr)
		println(maybe_color(term.bright_yellow, 'No Matches found'))
	} else if verbose || header {
		print(fdr)
		println(maybe_color(term.bright_green, '$fdr.matches.len matches Found\n'))
		for result in fdr.matches {
			result.show()
		}
	} else {
		for result in fdr.matches {
			result.show()
		}
	}
}

fn (fdr Finder) str() string {
	v := maybe_color(term.bright_red, '$fdr.visib')
	m := maybe_color(term.bright_red, '$fdr.mutab')
	st := if fdr.receiver != '' { ' ( _ $fdr.receiver)' } else { '' }
	s := maybe_color(term.bright_magenta, '$fdr.symbol')
	n := maybe_color(term.bright_cyan, '$fdr.name')

	mm := if fdr.modul != '' { maybe_color(term.blue, '$fdr.modul') } else { '' }
	dd := if fdr.dirs.len != 0 {
		fdr.dirs.map(maybe_color(term.blue, it))
	} else {
		fdr.dirs
	}

	dm := if fdr.dirs.len == 0 && fdr.modul == '' {
		'all the project scope'
	} else if fdr.dirs.len == 0 && fdr.modul != '' {
		'module $mm'
	} else if fdr.dirs.len != 0 && fdr.modul == '' {
		'directories: $dd'
	} else {
		'module $mm searching within directories: $dd'
	}

	return '\nFind: $s$st $n | visibility: $v mutability: $m\nwithin $dm '
}

// Match is one result of the search_for_matches() process
struct Match {
	path string [required]
	line int    [required]
	text string [required]
}

fn (mtc Match) show() {
	path := maybe_color(term.bright_magenta, mtc.path)
	line := maybe_color(term.bright_yellow, '$mtc.line')
	text := maybe_color(term.bright_green, '$mtc.text')
	if verbose || format {
		println('$path\n$line : [ $text ]\n')
	} else {
		println('$path:$line: $text')
	}
}
