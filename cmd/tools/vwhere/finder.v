module main

import os
import term
import os.cmdline

// Finder is entity that contains all the logic
struct Finder {
mut:
	symbol  Symbol
	visib   Visibility
	mutab   Mutability
	name    string
	modul   string
	mth_of  string
	dirs    []string
	matches []Match
}

fn (mut fdr Finder) configure_from_arguments() {
	args := os.args[2..]
	match args.len {
		1 {
			fdr.name = args[0]
		}
		2 {
			fdr.symbol.set_from_str(args[0])
			fdr.name = args[1]
		}
		else {
			fdr.symbol.set_from_str(args[0])
			fdr.name = args[1]
			fdr.visib.set_from_str(cmdline.option(args, '-vis', '${Visibility.all}'))
			fdr.mutab.set_from_str(cmdline.option(args, '-mut', '${Mutability.any}'))
			if fdr.symbol != .var && fdr.mutab != .any {
				make_and_print_error('-mut $fdr.mutab just can be setted with symbol_type:',
					['var'], '$fdr.symbol')
			}
			fdr.modul = cmdline.option(args, '-mod', '')
			fdr.mth_of = cmdline.option(args, '-m-of', '')
			if fdr.symbol !in [.@fn, .regexp] && fdr.mth_of != '' {
				make_and_print_error('-m-of $fdr.mth_of just can be setted with symbol_types:',
					['fn', 'regexp'], '$fdr.symbol')
			}
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
		if vroot !in paths_to_search {
			paths_to_search << vroot
		}
		paths_to_search << vmod_paths
	} else if fdr.dirs.len == 0 && fdr.modul != '' {
		paths_to_search << if fdr.modul == 'main' { current_dir } else { fdr.modul }
	} else if fdr.dirs.len != 0 && fdr.modul == '' {
		recursive = false
		paths_to_search << fdr.dirs
	} else {
		recursive = false
		paths_to_search << if fdr.modul == 'main' { current_dir } else { fdr.modul }
		paths_to_search << fdr.dirs
	}
	mut files_to_search := []string{}
	for path in paths_to_search {
		files_to_search << collect_v_files(path, recursive) or { panic(err) }
	}

	// Build regex query
	sp := r'\s*'
	op := r'\('
	cp := r'\)'
	vi := match fdr.visib {
		.all { '.*' }
		.@pub { '.*pub$sp' }
		.pri { '(?!.*pub)$sp' } // Thank you @btiffin#4552
	}
	mu := match fdr.mutab {
		.any { '.*' }
		.yes { 'mut$sp' }
		.not { '(?!mut)$sp' }
	}
	sy := '$fdr.symbol'
	st := if fdr.mth_of != '' { '$op$sp[a-z].*$sp$fdr.mth_of$cp$sp' } else { '.*' }
	na := '[ |\t]$fdr.name'

	query := match fdr.symbol {
		.regexp {
			if fdr.visib == .all { '$mu$na' } else { '$vi$mu$na' }
		}
		.var {
			if fdr.visib == .all { '$mu$na$sp:=.*' } else { '$vi$mu$na$sp:=.*' }
		}
		.@const {
			'$vi$na $sp=.*'
		}
		.@fn {
			'$vi$sy$st$na$sp${op}.*${cp}.*'
		}
		else {
			'$vi$sy$na .*' // for struct, enum and interface
		}
	}
	is_const := fdr.symbol == .@const
	for file in files_to_search {
		n_line, line := search_within_file(file, query, is_const)
		if n_line != 0 {
			fdr.matches << Match{file, n_line, line}
		}
	}
}

fn (fdr Finder) show_results() {
	print(fdr)
	if fdr.matches.len < 1 {
		println(maybe_color(term.bright_yellow, 'No Matches found'))
	} else {
		println(maybe_color(term.bright_green, '$fdr.matches.len matches Found\n'))
		for result in fdr.matches {
			result.show()
		}
	}
}

fn (fdr Finder) str() string {
	v := maybe_color(term.bright_red, '$fdr.visib')
	m := maybe_color(term.bright_red, '$fdr.mutab')
	st := if fdr.mth_of != '' { ' ( $fdr.mth_of)' } else { '' }
	s := maybe_color(term.bright_magenta, '$fdr.symbol')
	n := maybe_color(term.bright_blue, '$fdr.name')

	mm := if fdr.modul != '' { maybe_color(term.bright_blue, '$fdr.modul') } else { '' }
	dd := if fdr.dirs.len != 0 {
		fdr.dirs.map(maybe_color(term.bright_blue, it))
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
	println('$path\n$line : [ $text ]\n')
}
