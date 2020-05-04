// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import internal.help
import os
import os.cmdline
import v.table
import v.doc
import v.pref
import v.util
import v.builder

const (
	simple_cmd                          = ['fmt', 'up'
	'self', 'test'
	'test-fmt', 'test-compiler', 'test-fixed', 'bin2v'
	'repl'
	'build-tools'
	'build-examples', 'build-vbinaries', 'setup-freetype'
	]
	list_of_flags_that_allow_duplicates = ['cc', 'd', 'define', 'cf', 'cflags']
	list_of_flags_with_param            = [
		'o'
		'output', 'd', 'define', 'b', 'backend', 'cc', 'os', 'target-os', 'arch', 'csource'
		'cf', 'cflags', 'path']
)

fn main() {
	args := os.args[1..]
	// args = 123
	if args.len == 0 || args[0] in ['-', 'repl'] {
		// Running `./v` without args launches repl
		if args.len == 0 {
			println('For usage information, quit V REPL using `exit` and use `v help`')
		}
		util.launch_tool(false, 'vrepl')
		return
	}
	if args.len > 0 && (args[0] in ['version', '-V', '-version', '--version'] || (args[0] ==
		'-v' && args.len == 1)) {
		// `-v` flag is for setting verbosity, but without any args it prints the version, like Clang
		println(util.full_v_version())
		return
	}
	args_and_flags := util.join_env_vflags_and_os_args()[1..]
	prefs, command := parse_args(args_and_flags)
	if prefs.is_verbose {
		println('command = "$command"')
		println(util.full_v_version())
	}
	if prefs.is_verbose {
		// println('args= ')
		// println(args) // QTODO
		// println('prefs= ')
		// println(prefs) // QTODO
	}
	// Start calling the correct functions/external tools
	// Note for future contributors: Please add new subcommands in the `match` block below.
	if command in simple_cmd {
		// External tools
		util.launch_tool(prefs.is_verbose, 'v' + command)
		return
	}
	match command {
		'help' {
			invoke_help_and_exit(args)
		}
		'new', 'init' {
			util.launch_tool(prefs.is_verbose, 'vcreate')
			return
		}
		'translate' {
			println('Translating C to V will be available in V 0.3')
			return
		}
		'search', 'install', 'update', 'remove' {
			util.launch_tool(prefs.is_verbose, 'vpm')
			return
		}
		'get' {
			println('V Error: Use `v install` to install modules from vpm.vlang.io')
			exit(1)
		}
		'symlink' {
			create_symlink()
			return
		}
		'doc' {
			if args.len == 1 {
				println('v doc [module]')
				exit(1)
			}
			table := table.new_table()
			println(doc.doc(args[1], table))
			return
		}
		else {}
	}
	if command in ['run', 'build-module'] || command.ends_with('.v') || os.exists(command) {
		// println('command')
		// println(prefs.path)
		builder.compile(command, prefs)
		return
	}
	eprintln('v $command: unknown command\nRun "v help" for usage.')
	exit(1)
}

fn parse_args(args []string) (&pref.Preferences, string) {
	mut res := &pref.Preferences{}
	mut command := ''
	mut command_pos := 0
	// for i, arg in args {
	for i := 0; i < args.len; i++ {
		arg := args[i]
		current_args := args[i..]
		match arg {
			'-v' {
				res.is_verbose = true
			}
			'-cg' {
				res.is_debug = true
			}
			'-repl' {
				res.is_repl = true
			}
			'-live' {
				res.is_livemain = true
			}
			'-sharedlive' {
				res.is_liveshared = true
				res.is_shared = true
			}
			'-shared' {
				res.is_shared = true
			}
			'-autofree' {
				res.autofree = true
			}
			'-compress' {
				res.compress = true
			}
			'-freestanding' {
				res.is_bare = true
			}
			'-prof', '-profile' {
				res.profile_file = cmdline.option(current_args, '-profile', '-')
				res.is_prof = true
				i++
			}
			'-prod' {
				res.is_prod = true
			}
			'-stats' {
				res.is_stats = true
			}
			'-obfuscate' {
				res.obfuscate = true
			}
			'-translated' {
				res.translated = true
			}
			'-showcc' {
				res.show_cc = true
			}
			'-usecache' {
				res.use_cache = true
			}
			'-keepc' {
				res.keep_c = true
			}
			'-x64' {
				res.backend = .x64
			}
			'-print_v_files' {
				res.print_v_files = true
			}
			'-os' {
				target_os := cmdline.option(current_args, '-os', '')
				i++
				target_os_kind := pref.os_from_string(target_os) or {
					if target_os == 'cross' {
						res.output_cross_c = true
						continue
					}
					println('unknown operating system target `$target_os`')
					exit(1)
				}
				res.os = target_os_kind
			}
			'-printfn' {
				res.printfn_list << cmdline.option(current_args, '-printfn', '')
				i++
			}
			'-cflags' {
				res.cflags += ' ' + cmdline.option(current_args, '-cflags', '')
				i++
			}
			'-define', '-d' {
				if current_args.len > 1 {
					define := current_args[1]
					parse_define(mut res, define)
				}
				i++
			}
			'-cc' {
				res.ccompiler = cmdline.option(current_args, '-cc', 'cc')
				i++
			}
			'-o' {
				res.out_name = cmdline.option(current_args, '-o', '')
				i++
			}
			'-b' {
				b := pref.backend_from_string(cmdline.option(current_args, '-b', 'c')) or {
					continue
				}
				res.backend = b
				i++
			}
			else {
				mut should_continue := false
				for flag_with_param in list_of_flags_with_param {
					if '-$flag_with_param' == arg {
						should_continue = true
						i++
						break
					}
				}
				if should_continue {
					continue
				}
				if !arg.starts_with('-') && command == '' {
					command = arg
					command_pos = i
				}
			}
		}
	}
	if command.ends_with('.v') || os.exists(command) {
		res.path = command
	} else if command == 'run' {
		res.is_run = true
		if command_pos > args.len {
			eprintln('v run: no v files listed')
			exit(1)
		}
		res.path = args[command_pos + 1]
		res.run_args = args[command_pos + 2..]
	}
	if command == 'build-module' {
		res.build_mode = .build_module
		res.path = args[command_pos + 1]
	}
	if res.is_verbose {
		println('setting pref.path to "$res.path"')
	}
	res.fill_with_defaults()
	return res, command
}

fn invoke_help_and_exit(remaining []string) {
	match remaining.len {
		0, 1 { help.print_and_exit('default') }
		2 { help.print_and_exit(remaining[1]) }
		else {}
	}
	println('V Error: Expected only one help topic to be provided.')
	println('For usage information, use `v help`.')
	exit(1)
}

fn create_symlink() {
	$if windows {
		return
	}
	vexe := pref.vexe_path()
	mut link_path := '/usr/local/bin/v'
	mut ret := os.exec('ln -sf $vexe $link_path') or {
		panic(err)
	}
	if ret.exit_code == 0 {
		println('Symlink "$link_path" has been created')
	} else if os.system("uname -o | grep -q \'[A/a]ndroid\'") == 0 {
		println('Failed to create symlink "$link_path". Trying again with Termux path for Android.')
		link_path = '/data/data/com.termux/files/usr/bin/v'
		ret = os.exec('ln -sf $vexe $link_path') or {
			panic(err)
		}
		if ret.exit_code == 0 {
			println('Symlink "$link_path" has been created')
		} else {
			println('Failed to create symlink "$link_path". Try again with sudo.')
		}
	} else {
		println('Failed to create symlink "$link_path". Try again with sudo.')
	}
}

fn parse_define(prefs mut pref.Preferences, define string) {
    define_parts := define.split('=')
    if define_parts.len == 1 {
        prefs.compile_defines << define
        prefs.compile_defines_all << define
        return
    }
    if define_parts.len == 2 {
        prefs.compile_defines_all << define_parts[0]
        match define_parts[1] {
            '0' {}
            '1' {
                prefs.compile_defines << define_parts[0]
            }
            else {
                println('V error: Unknown define argument value `${define_parts[1]}` for ${define_parts[0]}.' +
                    'Expected `0` or `1`.')
                exit(1)
            }
        }
        return
    }
    println('V error: Unknown define argument: ${define}. Expected at most one `=`.')
    exit(1)
}
