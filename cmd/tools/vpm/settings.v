module main

import os
import os.cmdline
import log
import v.vmod

struct VpmSettings {
mut:
	is_help               bool
	is_once               bool
	is_verbose            bool
	is_force              bool
	is_local              bool
	server_urls           []string
	mirror_urls           []string
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

	global_vmodules_path := os.vmodules_dir()
	mut vmodules_path := global_vmodules_path.clone()
	is_local := '-l' in opts || '--local' in opts
	if is_local {
		wrkdir := os.getwd()
		mut mcache := vmod.get_cache()
		vmod_file_location := mcache.get_by_folder(wrkdir)
		project_root_dir := if vmod_file_location.vmod_file.len == 0 {
			wrkdir
		} else {
			vmod_file_location.vmod_folder
		}
		vmodules_path = os.join_path(project_root_dir, 'modules')
		verbose_println('init_settings, local installation, wrkdir: ${wrkdir} | project_root_dir: ${project_root_dir} | vmodules_path: ${vmodules_path}')
	}
	verbose_println('init_settings, final is_local: ${is_local} | vmodules_path: `${vmodules_path}`')

	is_no_inc := os.getenv('VPM_NO_INCREMENT') != ''
	is_dbg := os.getenv('VPM_DEBUG') != ''
	is_ci := os.getenv('CI') != ''

	mut logger := &log.Log{}
	logger.set_output_stream(os.stderr())
	if is_dbg {
		logger.set_level(.debug)
	}
	if !is_ci && !is_dbg {
		// Log by default, but only in the global location, no matter if --local was passed:
		cache_path := os.join_path(global_vmodules_path, '.cache')
		os.mkdir_all(cache_path, mode: 0o700) or { panic(err) }
		logger.set_output_path(os.join_path(cache_path, 'vpm.log'))
	}

	return VpmSettings{
		is_help:               '-h' in opts || '--help' in opts || 'help' in cmds
		is_once:               '--once' in opts
		is_verbose:            '-v' in opts || '--verbose' in opts
		is_force:              '-f' in opts || '--force' in opts
		is_local:              is_local
		server_urls:           get_server_urls_from_args(args)
		mirror_urls:           get_mirror_urls_from_args(args)
		vcs:                   if '--hg' in opts { .hg } else { .git }
		vmodules_path:         vmodules_path
		tmp_path:              os.join_path(os.vtmp_dir(), 'vpm_modules')
		no_dl_count_increment: is_ci || is_no_inc
		fail_on_prompt:        os.getenv('VPM_FAIL_ON_PROMPT') != ''
		logger:                logger
	}
}

fn get_server_urls_from_args(args []string) []string {
	mut server_urls := []string{}
	server_urls << cmdline.options(args, '-server-url')
	server_urls << cmdline.options(args, '--server-url')
	server_urls << cmdline.options(args, '--server-urls')
	return unique_server_urls(server_urls)
}

fn get_mirror_urls_from_args(args []string) []string {
	mut mirror_urls := []string{}
	mirror_urls << cmdline.options(args, '-m')
	mirror_urls << cmdline.options(args, '--mirror')
	return unique_server_urls(mirror_urls)
}

fn unique_server_urls(urls []string) []string {
	mut unique_urls := []string{}
	for raw_url in urls {
		url := normalize_server_url(raw_url)
		if url == '' || url in unique_urls {
			continue
		}
		unique_urls << url
	}
	return unique_urls
}

fn normalize_server_url(url string) string {
	return url.trim_space().trim_string_right('/')
}

fn parse_server_urls(args []string) []string {
	mut urls := []string{}
	mut i := 0
	for i < args.len {
		if args[i] in server_url_option_names {
			i++
			if i < args.len {
				url := normalize_server_url(args[i])
				if url != '' && url !in urls {
					urls << url
				}
			}
		}
		i++
	}
	return urls
}
