module main

import os
import net.http
import term
import log

[params]
struct ErrorOptions {
	details string
	verbose bool // is used to only output the error message if the verbose setting is enabled.
}

fn get_working_server_url() string {
	server_urls := if settings.server_urls.len > 0 {
		settings.server_urls
	} else {
		vpm_server_urls
	}
	for url in server_urls {
		verbose_println('Trying server url: ${url}')
		http.head(url) or {
			verbose_println('                   ${url} failed.')
			continue
		}
		return url
	}
	panic('No responding vpm server found. Please check your network connectivity and try again later.')
}

fn ensure_vmodules_dir_exist() {
	if !os.is_dir(settings.vmodules_path) {
		println('Creating `${settings.vmodules_path}` ...')
		os.mkdir(settings.vmodules_path) or {
			vpm_error(err.msg(), verbose: true)
			exit(1)
		}
	}
}

fn (vcs &VCS) is_executable() ! {
	os.find_abs_path_of_executable(vcs.cmd) or {
		return error('VPM needs `${vcs.cmd}` to be installed.')
	}
}

fn vcs_used_in_dir(dir string) ?VCS {
	for vcs in supported_vcs.values() {
		if os.is_dir(os.real_path(os.join_path(dir, vcs.dir))) {
			return vcs
		}
	}
	return none
}

fn verbose_println(msg string) {
	if settings.is_verbose {
		println(msg)
	}
}

fn vpm_log(line string, func string, msg string) {
	log.debug('${line} | (${func}) ${msg}')
}

fn vpm_error(msg string, opts ErrorOptions) {
	if opts.verbose && !settings.is_verbose {
		return
	}
	eprintln(term.ecolorize(term.red, 'error: ') + msg)
	if opts.details.len > 0 && settings.is_verbose {
		eprint(term.ecolorize(term.blue, 'details: '))
		padding := ' '.repeat('details: '.len)
		for i, line in opts.details.split_into_lines() {
			if i > 0 {
				eprint(padding)
			}
			eprintln(term.ecolorize(term.blue, line))
		}
	}
}
