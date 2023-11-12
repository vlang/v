module main

import os
import os.cmdline
import term
import v.doc
import v.vmod

const vexe = os.getenv_opt('VEXE') or { @VEXE }

const vroot = os.dir(vexe)

const allowed_formats = ['md', 'markdown', 'json', 'text', 'stdout', 'html', 'htm']

fn main() {
	if os.args.len < 2 || '-h' in os.args || '-help' in os.args || '--help' in os.args
		|| os.args[1..] == ['doc', 'help'] {
		os.system('${os.quoted_path(vexe)} help doc')
		exit(0)
	}
	args := os.args[2..].clone()
	cfg := parse_arguments(args)
	if cfg.input_path.len == 0 {
		eprintln('vdoc: No input path found.')
		exit(1)
	}
	// Config is immutable from this point on
	mut vd := &VDoc{
		cfg: cfg
		manifest: vmod.Manifest{
			repo_url: ''
		}
	}
	vd.vprintln('Setting output type to "${cfg.output_type}"')
	vd.generate_docs_from_file()
	if cfg.run_examples {
		println('')
		if vd.example_oks == 0 && vd.example_failures == 0 {
			println(term.colorize(term.bright_yellow, 'Found NO examples.'))
		} else {
			println(term.colorize(term.gray, 'Found ${vd.example_oks} ok examples.'))
		}
		if vd.example_failures > 0 {
			println(term.colorize(term.red, 'Found ${vd.example_failures} failing examples.'))
			exit(1)
		}
	}
}

fn parse_arguments(args []string) Config {
	mut cfg := Config{}
	cfg.is_color = term.can_show_color_on_stdout()
	for i := 0; i < args.len; i++ {
		arg := args[i]
		current_args := args[i..]
		match arg {
			'-all' {
				cfg.pub_only = false
			}
			'-f' {
				format := cmdline.option(current_args, '-f', '')
				if format !in allowed_formats {
					allowed_str := allowed_formats.join(', ')
					eprintln('vdoc: "${format}" is not a valid format. Only ${allowed_str} are allowed.')
					exit(1)
				}
				cfg.output_type = set_output_type_from_str(format)
				i++
			}
			'-color' {
				cfg.is_color = true
			}
			'-no-color' {
				cfg.is_color = false
			}
			'-inline-assets' {
				cfg.inline_assets = true
			}
			'-theme-dir' {
				cfg.theme_dir = cmdline.option(current_args, '-theme-dir', default_theme)
			}
			'-l' {
				cfg.show_loc = true
			}
			'-comments' {
				cfg.include_comments = true
			}
			'-m' {
				cfg.is_multi = true
			}
			'-o' {
				opath := cmdline.option(current_args, '-o', '')
				cfg.output_path = if opath == 'stdout' { opath } else { os.real_path(opath) }
				i++
			}
			'-os' {
				platform_str := cmdline.option(current_args, '-os', '')
				if platform_str == 'cross' {
					eprintln('`v doc -os cross` is not supported yet.')
					exit(1)
				}
				selected_platform := doc.platform_from_string(platform_str) or {
					eprintln(err.msg())
					exit(1)
				}
				cfg.platform = selected_platform
				i++
			}
			'-run-examples' {
				cfg.run_examples = true
			}
			'-no-timestamp' {
				cfg.no_timestamp = true
			}
			'-no-examples' {
				cfg.include_examples = false
			}
			'-readme' {
				cfg.include_readme = true
			}
			'-v' {
				cfg.is_verbose = true
			}
			else {
				if cfg.input_path.len < 1 {
					cfg.input_path = arg
				} else if !cfg.is_multi {
					// Symbol name filtering should not be enabled
					// in multi-module documentation mode.
					cfg.symbol_name = arg
				}
				if i == args.len - 1 {
					break
				}
			}
		}
	}
	// Correct from configuration from user input
	if cfg.output_path == 'stdout' && cfg.output_type == .html {
		cfg.inline_assets = true
	}
	$if windows {
		cfg.input_path = cfg.input_path.replace('/', os.path_separator)
	} $else {
		cfg.input_path = cfg.input_path.replace('\\', os.path_separator)
	}
	is_path := cfg.input_path.ends_with('.v') || cfg.input_path.split(os.path_separator).len > 1
		|| cfg.input_path == '.'
	if cfg.input_path.trim_right('/') == 'vlib' {
		cfg.is_vlib = true
		cfg.is_multi = true
		cfg.input_path = os.join_path(vroot, 'vlib')
	} else if !is_path {
		// TODO vd.vprintln('Input "$cfg.input_path" is not a valid path. Looking for modules named "$cfg.input_path"...')
		mod_path := doc.lookup_module(cfg.input_path) or {
			eprintln('vdoc: ${err}')
			exit(1)
		}
		cfg.input_path = mod_path
	}
	return cfg
}
