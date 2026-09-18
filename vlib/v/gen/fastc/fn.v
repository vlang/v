		}
		if (arg.ends_with('.o') || arg.ends_with('.a') || arg.ends_with('.obj') || arg.ends_with('.lib')) && !os.is_abs_path(arg) {
			args[i] = os.join_path(base_dir, arg)
		}
		resolved_arg := args[i]
		if resolved_arg.ends_with('.o') && !os.is_file(resolved_arg) {
			c_source := resolved_arg[..resolved_arg.len - 2] + '.c'
			if os.is_file(c_source) {
				args[i] = c_source
			}
		}
	}
	return args
}

fn fastc_pkgconfig_flags(raw string) ![]string {
	args := pref.pkgconfig_flags_args(raw) or {
		return error('fastc parser cannot split `#pkgconfig ${raw}`')
	}
	if args.len == 0 {
		return error('fastc parser requires a package name after `#pkgconfig`')
	}
	result := cmdexec.run('pkg-config', args)
	if result.exit_code != 0 {
		return error('fastc parser cannot resolve `#pkgconfig ${raw}`: ${result.output.trim_space()}')
	}
	return cmdexec.split_args(result.output.trim_space()) or {
		return error('fastc parser cannot split flags for `#pkgconfig ${raw}`')
	}
}

// fastc_flag_is_skippable reports whether a `#flag` payload only names link
// libraries/search paths/frameworks or header include paths. Such flags affect
// linking or header lookup, not C generation, so FastC can safely skip them (it
// manages its own tcc link line and ships the system headers these C files include).
// Anything else (`-D`, `-std=...`) still reaches the unsupported path.
