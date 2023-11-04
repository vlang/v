module main

import os
import os.cmdline

struct VpmSettings {
mut:
	is_help               bool
	is_once               bool
	is_verbose            bool
	server_urls           []string
	vcs                   string
	vmodules_path         string
	no_dl_count_increment bool
}

fn init_settings() VpmSettings {
	args := os.args[1..]
	opts := cmdline.only_options(args)
	cmds := cmdline.only_non_options(args)
	return VpmSettings{
		is_help: '-h' in opts || '--help' in opts || 'help' in cmds
		is_once: '--once' in opts
		is_verbose: '-v' in opts
		vcs: if '--hg' in opts { 'hg' } else { 'git' }
		server_urls: cmdline.options(args, '--server-urls')
		vmodules_path: os.vmodules_dir()
		no_dl_count_increment: os.getenv('VPM_NO_INCREMENT') == '1'
	}
}
