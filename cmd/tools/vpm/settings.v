module main

import os
import os.cmdline
import log

struct VpmSettings {
mut:
	is_help               bool
	is_once               bool
	is_verbose            bool
	is_force              bool
	server_urls           []string
	vmodules_path         string
	tmp_path              string
	no_dl_count_increment bool
	// To ensure that some test scenarios with conflicting module directory names do not get stuck in prompts.
	// It is intended that VPM does not display a prompt when `VPM_FAIL_ON_PROMPT` is set.
	fail_on_prompt bool
	// git is used by default. URL installations can specify `--hg`. For already installed modules
	// and VPM modules that specify a different VCS in their `v.mod`, the VCS is validated separately.
	vcs    VCS
	logger &log.Logger
}

fn init_settings() VpmSettings {
	args := os.args[1..]
	opts := cmdline.only_options(args)
	cmds := cmdline.only_non_options(args)

	vmodules_path := os.vmodules_dir()
	is_no_inc := os.getenv('VPM_NO_INCREMENT') != ''
	is_dbg := os.getenv('VPM_DEBUG') != ''
	is_ci := os.getenv('CI') != ''

	mut logger := &log.Log{}
	logger.set_output_stream(os.stderr())
	if is_dbg {
		logger.set_level(.debug)
	}
	if !is_ci && !is_dbg {
		// Log by default:
		cache_path := os.join_path(vmodules_path, '.cache')
		os.mkdir_all(cache_path, mode: 0o700) or { panic(err) }
		logger.set_output_path(os.join_path(cache_path, 'vpm.log'))
	}

	return VpmSettings{
		is_help:               '-h' in opts || '--help' in opts || 'help' in cmds
		is_once:               '--once' in opts
		is_verbose:            '-v' in opts || '--verbose' in opts
		is_force:              '-f' in opts || '--force' in opts
		server_urls:           cmdline.options(args, '--server-urls')
		vcs:                   if '--hg' in opts { .hg } else { .git }
		vmodules_path:         vmodules_path
		tmp_path:              os.join_path(os.vtmp_dir(), 'vpm_modules')
		no_dl_count_increment: is_ci || is_no_inc
		fail_on_prompt:        os.getenv('VPM_FAIL_ON_PROMPT') != ''
		logger:                logger
	}
}
