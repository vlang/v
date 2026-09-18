fn prepare_c_flags_for_link(flags []string, environment_c_flags []string, optimization_flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, c_compiler string, use_platform_non_c_compiler bool, uncached_dir string, mut stats CObjectCacheStats) ![]string {
	// Nothing to cache: without object-file or native-source flags the link
	// plan adds no value, and preparing it costs a compiler-identity probe
	// (subprocess) plus plan-file signatures on every build.
	mut has_cacheable_flag := false
	for flag in flags {
		clean := flag.trim_space()
		if c_flag_is_object_file(clean) || clean.ends_with('.mm') {
			has_cacheable_flag = true
			break
		}
	}
	if !has_cacheable_flag {
		mut passthrough := flags.clone()
		if c_link_flags_use_cpp_language(passthrough) {
			add_c_language_runtime_link_flags(mut passthrough, flags, 'c++', target)
		}
		if c_link_flags_use_objective_c_language(passthrough) {
			add_c_language_runtime_link_flags(mut passthrough, flags, 'objective-c', target)
		}
		return passthrough
	}
	mut prepared := []string{}
	mut active_language := ''
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		clean := flag.trim_space()
		if clean == '-x' {
			active_language = if i + 1 < flags.len { flags[i + 1].trim_space() } else { '' }
			prepared << flag
			if i + 1 < flags.len {
				prepared << flags[i + 1]
			}
			i += 2
			continue
		}
		if c_flag_is_object_file(clean) {
			stats.requests++
			adjacent_language := if !os.exists(clean) {
				if source_file := c_source_from_object_file(clean) {
					c_source_language(source_file, active_language)
				} else {
					''
				}
			} else {
				''
			}
			object_path := ensure_c_object_file(clean, active_language, support_flags, c99, pic_flag, target_args, target, c_compiler, use_platform_non_c_compiler, uncached_dir, mut stats)!
			append_c_link_object(mut prepared, object_path, active_language)
			add_c_language_runtime_link_flags(mut prepared, flags, adjacent_language, target)
		} else if clean.ends_with('.mm') {
			stats.requests++
			language := c_source_language(clean, active_language)
			object_path := ensure_c_source_object(clean, active_language, support_flags, c99, pic_flag, target_args, target, c_compiler, use_platform_non_c_compiler, uncached_dir, mut stats)!
			append_c_link_object(mut prepared, object_path, active_language)
			if c_generated_native_source_context(clean, uncached_dir) {
				os.rm(clean) or {}
			}
			add_c_language_runtime_link_flags(mut prepared, flags, language, target)
		} else if c_flag_is_c_source_file(clean) {
			prepared << flag
		} else {
			prepared << flag
		}
		i++
	}
fn c_link_plan_path(cache_dir string, flags []string, support_flags []string, c99 bool, pic_flag string, target_args []string, target pref.Target, compiler string, use_platform_non_c_compiler bool, mut stats CObjectCacheStats) string {
	compiler_path, compiler_version := c_object_compiler_identity(compiler, mut stats)
	mut hash := u64(1469598103934665603)
	for identity in ['v3-c-link-plan-v3', os.getwd(), flags.join('\x00'), support_flags.join('\x00'),
		c99.str(), pic_flag, target_args.join('\x00'), compiler_path, compiler_version, target.os,
		target.arch, target.abi, target.endian, target.pointer_bits.str(), target.object_format,
		use_platform_non_c_compiler.str()] {
		hash = c_hash_bytes(hash, identity.bytes())
		hash = c_hash_bytes(hash, [u8(0xff)])
	}
	return os.join_path(cache_dir, 'link_${hash.hex()}.manifest')
}

fn valid_c_link_plan(plan_path string, mut stats CObjectCacheStats) ?CLinkPlan {
	content := os.read_file(plan_path) or { return none }
	lines := content.split_into_lines()
	if lines.len < 5 || lines[0] != 'format=v3-c-link-plan-v3' {
		return none
	}
	mut plan := CLinkPlan{}
	mut objects := []string{}
	mut complete := false
	mut saw_requests := false
fn write_c_link_plan(plan_path string, flags []string, stats &CObjectCacheStats) ! {
	mut out := strings.new_builder(256 + flags.len * 64 + stats.file_signatures.len * 96)
	out.writeln('format=v3-c-link-plan-v3')
	out.writeln('requests=${stats.requests}')
	out.writeln('direct_objects=${stats.direct_objects}')
	out.writeln('dependency_files=${stats.dependency_files}')
	for flag in flags {
		out.writeln('flag=${flag}')
		if c_flag_is_object_file(flag.trim_space()) && os.is_file(flag.trim_space()) {
			out.writeln('object=${flag.trim_space()}')
		}
	}
	mut dependencies := stats.file_signatures.keys()
	dependencies.sort()
	for dependency in dependencies {
		metadata := modulecache.file_metadata_signature(dependency)
		out.writeln('dependency=${dependency}\t${metadata}\t${stats.file_signatures[dependency]}')
	}
fn c_flag_consumes_next_operand(flag string) bool {
	return flag in ['-I', '-L', '-F', '-D', '-U', '-include', '-imacros', '-isystem', '-iquote',
		'-idirafter', '-iprefix', '-iwithprefix', '-iwithprefixbefore', '-isysroot', '--sysroot',
		'-target', '-arch', '-framework', '-weak_framework', '-Xlinker', '-force_load', '-o', '-MF',
		'-MT', '-MQ']
}

fn c_flag_is_existing_file(flag string) bool {
	clean := flag.trim(' \t\r\n"\'')
	return clean.len > 0 && clean[0] != `-` && os.is_file(clean)
}
fn tcc_native_c_source_flags(flags []string) []string {
	mut sources := []string{}
	mut language := ''
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		clean := flag.trim(' \t\r\n"\'')
		if clean == '-x' {
			language = if i + 1 < flags.len { flags[i + 1].trim_space() } else { '' }
			i += 2
			continue
		}
		if c_flag_consumes_next_operand(clean) || clean in ['-l', '-weak_library'] {
			i += 2
			continue
		}
		if clean.ends_with('.c') {
			sources << flag
		} else if language == 'c' && clean.len > 0 && !clean.starts_with('-') {
			// Preserve explicit language selection for extensionless inputs, then
			// reset it before the following cached dylib argument.
			sources << ['-x', 'c', flag, '-x', 'none']
		}
		i++
	}
	return sources
}
fn c_flag_token_is_link_only(token string) bool {
	clean := token.trim(' \t\r\n"\'')
	if clean.starts_with('-l') || clean.starts_with('-L') || clean.starts_with('-Wl,')
		|| clean in ['-ObjC', '-all_load', '-bundle', '-dynamiclib', '-shared', '-static', '-rdynamic',
			'-pie', '-no-pie'] {
		return true
	}
	return clean.ends_with('.a') || clean.ends_with('.so') || clean.contains('.so.')
		|| clean.ends_with('.dylib') || clean.ends_with('.dll') || clean.ends_with('.lib')
		|| clean.ends_with('.tbd')
}
fn c_flag_is_object_file(flag string) bool {
	return flag.ends_with('.o') || flag.ends_with('.obj')
}

fn c_flag_is_c_source_file(flag string) bool {
	return flag.ends_with('.c') || flag.ends_with('.cc') || flag.ends_with('.cpp')
		|| flag.ends_with('.m') || flag.ends_with('.mm')
}

fn c_standard_flag(c99 bool) string {
	return if c99 { '-std=c99' } else { '-std=gnu11' }
}
