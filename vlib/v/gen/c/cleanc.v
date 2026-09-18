		dir = os.getwd()
	}
	return util.nearest_vmod_root(source_file) or { os.real_path(dir) }
}

fn c_pkgconfig_flags(raw string) []string {
	packages := cmdexec.split_args(trimmed_space(raw)) or { return []string{} }
	if packages.len == 0 {
		return []string{}
	}
	mut args := ['--cflags', '--libs']
	args << packages
	result := cmdexec.run('pkg-config', args)
	if result.exit_code != 0 {
		return []string{}
	}
	return cmdexec.split_args(trimmed_space(result.output)) or { []string{} }
}
