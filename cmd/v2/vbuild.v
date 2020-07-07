module main

import cli
import os
import v.builder
import v.pref

fn build_cmd() cli.Command {
	return cli.Command{
		name: 'build'
		usage: '<file>'
		description: 'Build V code in the provided path'
		flags: build_flags()
		execute: build_cmd_func
	}
}

fn build_cmd_func(cmd cli.Command)? {
	mut prefs := parse_build_preferences(cmd.flags)?

	if cmd.args.len == 1 && (cmd.args[0].ends_with('.v') || os.exists(cmd.args[0])) {
		prefs.path = cmd.args[0]
		prefs.run_args = cmd.args[0..]
		prefs.fill_with_defaults() // TODO: defaults should be set before applying user input

		builder.compile(cmd.args[0], prefs)	
	} else {
		panic('no V source code file found')
	}
}

fn build_flags() []cli.Flag {
	mut flags := []cli.Flag{}
	flags << cli.Flag{
		flag: .string
		name: 'os'
		description: 'Set target os of compiled binary
									Currently available targets: linux, windows, mac, freebsd, openbsd, netbsd, dragonfly, js, solaris, android, haiku, auto'
	}
	flags << cli.Flag{
		flag: .string
		name: 'cc'
		description: 'Set the c compiler to be using to compile the binary'
	}
	flags << cli.Flag{
		flag: .string
		name: 'custom-prelude'
		description: 'Specify location of a custom prelude file'
	}
	flags << cli.Flag{
		flag: .string
		name: 'o'
		description: 'Specify output location of compiled binary'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'cg'
		description: 'Compile binary in debug mode with debugging symbols enabled'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'prod'
		description: 'Compile binary in production mode with optimizations enabled'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'obfuscate'
		description: 'Enable obfuscation of compiled binary
									Currently only renames symbols'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'compress'
		description: 'Enable striping of compiled binary'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'shared'
		description: 'Compile binary as a dynamic library'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'live'
		description: 'Enable live code reloading for tagged functions'
	}
	flags << cli.Flag{	// TODO: remove because redundant to '-shared -live'
		flag: .bool
		name: 'sharedlive'
		description: 'Enabled live code reloading for tagged functions in a dynamic library'
	}
	flags << cli.Flag{
		flag: .string
		name: 'path'
		description: 'Specify paths looked up for importing dependencies
									paths must be sepearted using pipes (\'|\'), default: \'@vlib|@vmodules\'
									\'@vmodules\' is replaced with the location of the global V modules folder (default: ~/.vmodules/)
									\'@vlib\' is replaced with the location of the V vlib folder'
	}
	flags << cli.Flag{
		flag: .string
		name: 'profile'
		description: 'Compile binary with all functions profiled'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'profile-no-inline'
		description: 'Skip \'[inline]\' functions when profiling'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'stats'
		description: 'Enable statistics reporting when compiling'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'translated'
		description: 'Enable features disallowed in regular V code but required for translated V code'
	}
	// flags << cli.Flag{
	// 	flag: .bool
	// 	name: 'keepc'
	// 	description: 'Forces the generated C file to be kept after compilation'
	// }
	flags << cli.Flag{
		flag: .bool
		name: 'print-files'
		description: 'Forces printing of all parsed V files without processing them
									This is useful for running external processing tools (e.g. etags or ctags)'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'print-cc' // changed from 'showcc'
		description: 'Print the C compiler command used for compiling the binary'
	}
	flags << cli.Flag{
		flag: .string
		name: 'print-function' // changed from printfn
		description: 'Print the compiled C code of the given function'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'w'
		description: 'Disables warnings when compiling the binary'
	}
	flags << cli.Flag{
		flag: .int
		name: 'error-limit'
		description: 'Set error limit for compilation'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'force-color'
		description: 'Forces the use of ANSI colors for error/warning messages'
	}
	flags << cli.Flag{
		flag: .bool
		name: 'disable-color'
		description: 'Disables the use of ANSI colors for error/warning messages'
	}
	return flags
}

fn parse_build_preferences(flags []cli.Flag) ?&pref.Preferences {
	mut prefs := &pref.Preferences{}
	for flag in flags.get_all_found() {
		match flag.name {
			'verbose' {
				prefs.is_verbose = flag.get_bool()?
			}
			'silent' {
				is_silent := flag.get_bool()? // TODO: fix once option types work in if
				if is_silent {
					prefs.output_mode = .silent
				}
			}
			'backend' {
				backend := flag.get_string()?
				prefs.backend = pref.backend_from_string(backend)?
			}
			'os' {
				os := flag.get_string()?
				prefs.os = pref.os_from_string(os)?
			}
			'cc' {
				prefs.ccompiler = flag.get_string()?
			}
			'custom-prelude' {
				path := flag.get_string()?
				prefs.custom_prelude = os.read_file(path) or {
					return error('failed to open custom prelude file: $err')
				}
			}
			'o' {
				prefs.out_name = flag.get_string()?
			}
			'debug' {
				prefs.is_debug = flag.get_bool()?
			}
			'production' {
				prefs.is_prod = flag.get_bool()?
			}
			'obfuscate' {
				prefs.obfuscate = flag.get_bool()?
			}
			'compress' {
				prefs.compress = flag.get_bool()?
			}
			'shared' {
				prefs.is_shared = flag.get_bool()?
			}
			'live' {
				prefs.is_livemain = flag.get_bool()?
			}
			'sharedlive' {
				prefs.is_liveshared = flag.get_bool()?
				prefs.is_shared = flag.get_bool()?
			}
			'path' {
				path := flag.get_string()?
				prefs.lookup_path = path.split(os.path_delimiter)
			}
			'profile' {
				prefs.profile_file = flag.get_string()?
				prefs.is_prof = true	
			}
			'profile-no-inline' {
				prefs.profile_no_inline = flag.get_bool()?
			}
			'stats' {
				prefs.is_stats = flag.get_bool()?
			}
			'translated' {
				prefs.translated = flag.get_bool()?
			}
			// 'keepc' {
			//	prefs.keepc = flag.get_bool()?
			// }
			'print-files' {
				prefs.print_v_files = flag.get_bool()?
			}
			'print-cc' {
				prefs.show_cc = flag.get_bool()?
			}
			'print-function' {
				functions := flag.get_string()?
				prefs.printfn_list << functions.split(',')
			}
			'w' {
				prefs.skip_warnings = flag.get_bool()?
			}
			'error-limit' {
				prefs.error_limit = flag.get_int()?
			}
			'force-color' {
				force_color := flag.get_bool()? // TODO: fix once option types work in if
				if force_color {
					prefs.use_color = .always
				}
			}
			'disable-color' {
				disable_color := flag.get_bool()? // TODO: fix once option types work in if
				if disable_color {
					prefs.use_color = .never
				}
			}
			
			else {
				continue
			}
		}
	}
	return prefs
}
