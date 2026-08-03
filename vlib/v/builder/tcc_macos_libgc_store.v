module builder

import crypto.sha256
import os
import sync

const tcc_macos_libgc_store_name = 'v-tcc-libgc-v1'
const tcc_macos_libgc_name = 'libgc.dylib'
const tcc_macos_libgc_max_size = u64(64 * 1024 * 1024)
const tcc_macos_libgc_hash_buffer_size = 256 * 1024
const tcc_macos_libgc_temp_attempts = 32
const tcc_macos_libgc_data_root_remediation = 'set XDG_DATA_HOME to an absolute, existing, private directory without a comma'

enum TccMacosLibgcArgList {
	linker_flags
	pre_args
}

struct TccMacosLibgcTokenLocation {
	arg_list TccMacosLibgcArgList
	index    int = -1
}

struct TccMacosLibgcPlan {
	required           bool
	source_dylib       string
	source_dylib_token string
	source_rpath_token string
	dylib_location     TccMacosLibgcTokenLocation
	rpath_location     TccMacosLibgcTokenLocation
}

struct TccMacosLibgcFileHash {
	sum  string
	size u64
}

struct TccMacosLibgcTemporaryObject {
	directory string
	path      string
}

struct TccMacosLibgcPublicationResult {
	final_dylib string
	won         bool
}

fn plan_tcc_macos_libgc_store(vroot string, linker_flags []string, pre_args []string) !TccMacosLibgcPlan {
	physical_vroot := os.real_path(vroot)
	if !physical_vroot.contains(',') {
		return TccMacosLibgcPlan{}
	}
	lib_dir := os.join_path(vroot, 'thirdparty', 'tcc', 'lib')
	source_dylib := os.join_path(lib_dir, tcc_macos_libgc_name)
	dylib_token := '"${source_dylib}"'
	rpath_token := '-Wl,-rpath,"${lib_dir}"'
	mut dylib_locations := []TccMacosLibgcTokenLocation{}
	mut rpath_locations := []TccMacosLibgcTokenLocation{}
	mut near_dylibs := 0
	mut near_rpaths := 0
	for i, token in linker_flags {
		if token == dylib_token {
			dylib_locations << TccMacosLibgcTokenLocation{
				arg_list: .linker_flags
				index:    i
			}
		} else if token == rpath_token {
			rpath_locations << TccMacosLibgcTokenLocation{
				arg_list: .linker_flags
				index:    i
			}
		} else if is_tcc_macos_libgc_dylib_near_match(token, source_dylib) {
			near_dylibs++
		} else if is_tcc_macos_libgc_rpath_near_match(token, lib_dir) {
			near_rpaths++
		}
	}
	for i, token in pre_args {
		if token == dylib_token {
			dylib_locations << TccMacosLibgcTokenLocation{
				arg_list: .pre_args
				index:    i
			}
		} else if token == rpath_token {
			rpath_locations << TccMacosLibgcTokenLocation{
				arg_list: .pre_args
				index:    i
			}
		} else if is_tcc_macos_libgc_dylib_near_match(token, source_dylib) {
			near_dylibs++
		} else if is_tcc_macos_libgc_rpath_near_match(token, lib_dir) {
			near_rpaths++
		}
	}
	if near_dylibs > 0 || near_rpaths > 0 {
		return error('ambiguous macOS bundled libgc flags: found ${near_dylibs} dylib and ${near_rpaths} rpath near-match(es)')
	}
	if dylib_locations.len == 0 && rpath_locations.len == 0 {
		return TccMacosLibgcPlan{}
	}
	if dylib_locations.len != 1 || rpath_locations.len != 1 {
		return error('ambiguous macOS bundled libgc flags: expected one exact dylib/rpath pair, found ${dylib_locations.len}/${rpath_locations.len}')
	}
	return TccMacosLibgcPlan{
		required:           true
		source_dylib:       source_dylib
		source_dylib_token: dylib_token
		source_rpath_token: rpath_token
		dylib_location:     dylib_locations[0]
		rpath_location:     rpath_locations[0]
	}
}

fn is_tcc_macos_libgc_dylib_near_match(token string, source_dylib string) bool {
	normalized := token.trim_space().trim('"').trim("'").replace('\\', '/')
	normalized_source := source_dylib.replace('\\', '/')
	return normalized.contains(normalized_source)
		|| normalized.contains('/thirdparty/tcc/lib/${tcc_macos_libgc_name}')
}

fn is_tcc_macos_libgc_rpath_near_match(token string, source_lib_dir string) bool {
	trimmed_token := token.trim_space().trim('"')
	mut rpath_value := ''
	for prefix in ['-Wl,-rpath,', '-Wl,--rpath,', '-Wl,-rpath=', '-Wl,--rpath='] {
		if trimmed_token.starts_with(prefix) {
			rpath_value = trimmed_token[prefix.len..]
			break
		}
	}
	if rpath_value == '' {
		return false
	}
	normalized := rpath_value.trim_space().trim('"').trim("'").replace('\\', '/')
	normalized_source := source_lib_dir.replace('\\', '/')
	bundled_lib_suffix := '/thirdparty/tcc/lib'
	return normalized.contains(normalized_source) || normalized.ends_with(bundled_lib_suffix)
		|| normalized.contains(bundled_lib_suffix + '/')
}

fn rewrite_tcc_macos_libgc_flags(plan TccMacosLibgcPlan, linker_flags []string, pre_args []string, stored_dylib string) !([]string, []string) {
	if !plan.required {
		return linker_flags.clone(), pre_args.clone()
	}
	if !tcc_macos_libgc_location_is_valid(plan.dylib_location, linker_flags, pre_args)
		|| !tcc_macos_libgc_location_is_valid(plan.rpath_location, linker_flags, pre_args) {
		return error('macOS bundled libgc rewrite plan contains an invalid token index')
	}
	if tcc_macos_libgc_token_at(plan.dylib_location, linker_flags, pre_args) != plan.source_dylib_token
		|| tcc_macos_libgc_token_at(plan.rpath_location, linker_flags, pre_args) != plan.source_rpath_token {
		return error('macOS bundled libgc flags changed after planning')
	}
	if !os.is_abs_path(stored_dylib) || os.file_name(stored_dylib) != tcc_macos_libgc_name {
		return error('macOS bundled libgc store returned an invalid dylib path')
	}
	stored_dir := os.dir(stored_dylib)
	if !tcc_macos_libgc_flag_path_is_safe(stored_dir) {
		return error('macOS bundled libgc store path cannot be represented safely in a linker rpath')
	}
	mut rewritten_linker_flags := linker_flags.clone()
	mut rewritten_pre_args := pre_args.clone()
	match plan.dylib_location.arg_list {
		.linker_flags { rewritten_linker_flags[plan.dylib_location.index] = '"${stored_dylib}"' }
		.pre_args { rewritten_pre_args[plan.dylib_location.index] = '"${stored_dylib}"' }
	}
	match plan.rpath_location.arg_list {
		.linker_flags {
			rewritten_linker_flags[plan.rpath_location.index] = '-Wl,-rpath,"${stored_dir}"'
		}
		.pre_args {
			rewritten_pre_args[plan.rpath_location.index] = '-Wl,-rpath,"${stored_dir}"'
		}
	}
	return rewritten_linker_flags, rewritten_pre_args
}

fn tcc_macos_libgc_location_is_valid(location TccMacosLibgcTokenLocation, linker_flags []string, pre_args []string) bool {
	if location.index < 0 {
		return false
	}
	return match location.arg_list {
		.linker_flags { location.index < linker_flags.len }
		.pre_args { location.index < pre_args.len }
	}
}

fn tcc_macos_libgc_token_at(location TccMacosLibgcTokenLocation, linker_flags []string, pre_args []string) string {
	return match location.arg_list {
		.linker_flags { linker_flags[location.index] }
		.pre_args { pre_args[location.index] }
	}
}

fn materialize_tcc_macos_libgc(source_dylib string) !string {
	data_base := resolve_tcc_macos_libgc_data_base(os.getenv('XDG_DATA_HOME'), os.getenv('HOME'))!
	return materialize_tcc_macos_libgc_at(source_dylib, data_base)
}

fn materialize_and_rewrite_tcc_macos_libgc_flags(plan TccMacosLibgcPlan, linker_flags []string, pre_args []string) !([]string, []string) {
	stored_dylib := materialize_tcc_macos_libgc(plan.source_dylib)!
	return rewrite_tcc_macos_libgc_flags(plan, linker_flags, pre_args, stored_dylib)
}

fn materialize_and_rewrite_tcc_macos_libgc_flags_at(plan TccMacosLibgcPlan, linker_flags []string, pre_args []string, data_base string) !([]string, []string) {
	stored_dylib := materialize_tcc_macos_libgc_at(plan.source_dylib, data_base)!
	return rewrite_tcc_macos_libgc_flags(plan, linker_flags, pre_args, stored_dylib)
}

fn materialize_tcc_macos_libgc_at(source_dylib string, requested_data_base string) !string {
	resolved_source := resolve_tcc_macos_libgc_source(source_dylib)!
	source_hash := hash_tcc_macos_libgc_regular_file(resolved_source)!
	data_base := resolve_tcc_macos_libgc_data_base(requested_data_base, '')!
	store_root := os.join_path(data_base, tcc_macos_libgc_store_name)
	ensure_tcc_macos_libgc_private_directory(store_root)!
	content_dir := os.join_path(store_root, source_hash.sum)
	ensure_tcc_macos_libgc_private_directory(content_dir)!
	final_dylib := os.join_path(content_dir, tcc_macos_libgc_name)
	_, final_exists := lstat_tcc_macos_libgc_path(final_dylib)!
	if final_exists {
		validate_tcc_macos_libgc_store_object(final_dylib, source_hash)!
		return final_dylib
	}
	temporary := copy_tcc_macos_libgc_to_exclusive_temp(resolved_source, content_dir, source_hash)!
	publication := publish_tcc_macos_libgc_temporary(temporary, final_dylib, source_hash)!
	return publication.final_dylib
}

fn resolve_tcc_macos_libgc_data_base(xdg_data_home string, home string) !string {
	if xdg_data_home != '' {
		return resolve_tcc_macos_libgc_existing_data_base(xdg_data_home)
	}
	if home == '' {
		return error(tcc_macos_libgc_data_root_error('XDG_DATA_HOME="" and HOME=""',
			'no persistent data root is configured'))
	}
	if !os.is_abs_path(home) {
		return error(tcc_macos_libgc_data_root_error(home, 'the path is relative'))
	}
	canonical_home := resolve_tcc_macos_libgc_existing_data_base(home)!
	local_data := ensure_tcc_macos_libgc_fallback_data_directory(os.join_path(canonical_home,
		'.local'))!
	ensure_tcc_macos_libgc_fallback_data_directory(os.join_path(local_data, 'share'))!
	return resolve_tcc_macos_libgc_existing_data_base(os.join_path(home, '.local', 'share'))
}

fn resolve_tcc_macos_libgc_existing_data_base(raw_base string) !string {
	if !os.is_abs_path(raw_base) {
		return error(tcc_macos_libgc_data_root_error(raw_base, 'the path is relative'))
	}
	_, raw_exists := lstat_tcc_macos_libgc_path(raw_base) or {
		return error(tcc_macos_libgc_data_root_error(raw_base,
			'the path cannot be inspected: ${err.msg()}'))
	}
	if !raw_exists {
		return error(tcc_macos_libgc_data_root_error(raw_base, 'the path does not exist'))
	}
	canonical_base := os.real_path(raw_base)
	if !os.is_abs_path(canonical_base) || !tcc_macos_libgc_flag_path_is_safe(canonical_base) {
		return error(tcc_macos_libgc_data_root_error('${raw_base} (canonical: ${canonical_base})',
			'the canonical path is not a safe comma-free absolute path'))
	}
	canonical_stat, canonical_exists := lstat_tcc_macos_libgc_path(canonical_base) or {
		return error(tcc_macos_libgc_data_root_error('${raw_base} (canonical: ${canonical_base})',
			'the canonical path cannot be inspected: ${err.msg()}'))
	}
	if !canonical_exists {
		return error(tcc_macos_libgc_data_root_error(raw_base,
			'the canonical path cannot be resolved'))
	}
	validate_tcc_macos_libgc_data_base_stat(canonical_stat,
		'${raw_base} (canonical: ${canonical_base})', os.geteuid())!
	return canonical_base
}

fn ensure_tcc_macos_libgc_fallback_data_directory(path string) !string {
	_, exists := lstat_tcc_macos_libgc_path(path)!
	if !exists {
		os.mkdir(path, mode: 0o700) or {
			_, concurrent_exists := lstat_tcc_macos_libgc_path(path)!
			if !concurrent_exists {
				return error(tcc_macos_libgc_data_root_error(path,
					'the default directory cannot be created: ${err.msg()}'))
			}
		}
	}
	return resolve_tcc_macos_libgc_existing_data_base(path)
}

fn tcc_macos_libgc_data_root_error(rejected string, reason string) string {
	return 'invalid macOS bundled libgc data root `${rejected}`: ${reason}; remediation: ${tcc_macos_libgc_data_root_remediation}'
}

fn tcc_macos_libgc_flag_path_is_safe(path string) bool {
	if path.contains(',') || path.contains('"') || path.contains('\\') || path.contains('`')
		|| path.contains('$') {
		return false
	}
	for b in path.bytes() {
		if b < 0x20 || b == 0x7f {
			return false
		}
	}
	return true
}

fn validate_tcc_macos_libgc_data_base_stat(stat os.Stat, path string, expected_euid int) ! {
	if stat.get_filetype() != .directory {
		return error(tcc_macos_libgc_data_root_error(path, 'the path is not a directory'))
	}
	if stat.uid != u32(expected_euid) {
		return error(tcc_macos_libgc_data_root_error(path,
			'the directory is not owned by the effective user'))
	}
	mode := stat.mode & 0o7777
	if (mode & 0o700) != 0o700 || (mode & 0o7022) != 0 {
		return error(tcc_macos_libgc_data_root_error(path,
			'the directory has unsafe permissions ${mode:o}'))
	}
}

fn validate_tcc_macos_libgc_private_directory_stat(stat os.Stat, expected_euid int) ! {
	if stat.get_filetype() != .directory {
		return error('macOS bundled libgc store path is not a directory')
	}
	if stat.uid != u32(expected_euid) {
		return error('macOS bundled libgc store directory is not owned by the effective user')
	}
	mode := stat.mode & 0o7777
	if mode != 0o700 {
		return error('macOS bundled libgc store directory must have mode 0700, got ${mode:o}')
	}
}

fn ensure_tcc_macos_libgc_private_directory(path string) ! {
	stat, exists := lstat_tcc_macos_libgc_path(path)!
	if exists {
		validate_tcc_macos_libgc_private_directory_stat(stat, os.geteuid())!
		return
	}
	os.mkdir(path, mode: 0o700) or {
		concurrent_stat, concurrent_exists := lstat_tcc_macos_libgc_path(path)!
		if !concurrent_exists {
			return error('cannot create private macOS bundled libgc store directory ${path}: ${err.msg()}')
		}
		validate_tcc_macos_libgc_private_directory_stat(concurrent_stat, os.geteuid())!
		return
	}
	created_stat, created_exists := lstat_tcc_macos_libgc_path(path)!
	if !created_exists {
		return error('private macOS bundled libgc store directory disappeared after creation: ${path}')
	}
	validate_tcc_macos_libgc_private_directory_stat(created_stat, os.geteuid())!
}

fn resolve_tcc_macos_libgc_source(source_dylib string) !string {
	if !os.is_abs_path(source_dylib) {
		return error('macOS bundled libgc source path must be absolute')
	}
	normalized_lib_dir := os.norm_path(os.dir(source_dylib)).replace('\\', '/').trim_right('/')
	if os.file_name(source_dylib) != tcc_macos_libgc_name
		|| !normalized_lib_dir.ends_with('/thirdparty/tcc/lib') {
		return error('macOS bundled libgc source is not the official thirdparty/tcc/lib object')
	}
	source_stat, source_exists := lstat_tcc_macos_libgc_path(source_dylib)!
	if !source_exists {
		return error('macOS bundled libgc source does not exist: ${source_dylib}')
	}
	if source_stat.get_filetype() !in [.regular, .symbolic_link] {
		return error('macOS bundled libgc source must be a regular file or an internal symlink')
	}
	if source_stat.get_filetype() == .symbolic_link {
		link_target := os.readlink(source_dylib)!
		if link_target == '' {
			return error('macOS bundled libgc source symlink has an empty target')
		}
	}
	lexical_lib_dir := os.dir(source_dylib)
	lexical_vroot := os.dir(os.dir(os.dir(lexical_lib_dir)))
	canonical_vroot := os.real_path(lexical_vroot)
	lib_dir := os.real_path(lexical_lib_dir)
	if !path_is_within_tcc_macos_libgc_dir(lib_dir, canonical_vroot) {
		return error('macOS bundled libgc source directory resolves outside the canonical VROOT')
	}
	resolved_source := os.real_path(source_dylib)
	if !path_is_within_tcc_macos_libgc_dir(resolved_source, lib_dir) {
		return error('macOS bundled libgc source symlink resolves outside thirdparty/tcc/lib')
	}
	resolved_stat, resolved_exists := lstat_tcc_macos_libgc_path(resolved_source)!
	if !resolved_exists || resolved_stat.get_filetype() != .regular {
		return error('macOS bundled libgc source target is not a regular file')
	}
	validate_tcc_macos_libgc_bounded_regular_stat(resolved_stat)!
	return resolved_source
}

fn path_is_within_tcc_macos_libgc_dir(path string, directory string) bool {
	if !os.is_abs_path(path) || !os.is_abs_path(directory) {
		return false
	}
	clean_directory := directory.trim_right(os.path_separator)
	return path.starts_with(clean_directory + os.path_separator)
}

fn validate_tcc_macos_libgc_bounded_regular_stat(stat os.Stat) ! {
	if stat.get_filetype() != .regular {
		return error('macOS bundled libgc object is not a regular file')
	}
	if stat.size == 0 || stat.size > tcc_macos_libgc_max_size {
		return error('macOS bundled libgc object size ${stat.size} is outside the accepted range')
	}
}

fn hash_tcc_macos_libgc_regular_file(path string) !TccMacosLibgcFileHash {
	before, before_exists := lstat_tcc_macos_libgc_path(path)!
	if !before_exists {
		return error('macOS bundled libgc object does not exist: ${path}')
	}
	validate_tcc_macos_libgc_bounded_regular_stat(before)!
	mut file := os.open(path)!
	defer {
		file.close()
	}
	mut digest := sha256.new()
	mut buffer := []u8{len: tcc_macos_libgc_hash_buffer_size}
	mut total := u64(0)
	for {
		read := file.read(mut buffer) or {
			if err is os.Eof {
				break
			}
			return error('cannot read macOS bundled libgc object ${path}: ${err.msg()}')
		}
		if read <= 0 {
			break
		}
		total += u64(read)
		if total > tcc_macos_libgc_max_size {
			return error('macOS bundled libgc object grew beyond the accepted size while hashing')
		}
		digest.write(buffer[..read])!
	}
	after, after_exists := lstat_tcc_macos_libgc_path(path)!
	if !after_exists || !same_tcc_macos_libgc_file_snapshot(before, after) || total != before.size {
		return error('macOS bundled libgc object changed while hashing: ${path}')
	}
	return TccMacosLibgcFileHash{
		sum:  digest.sum([]).hex()
		size: total
	}
}

fn same_tcc_macos_libgc_file_snapshot(first os.Stat, second os.Stat) bool {
	return first.dev == second.dev && first.inode == second.inode && first.mode == second.mode
		&& first.uid == second.uid && first.size == second.size && first.mtime == second.mtime
}

fn validate_tcc_macos_libgc_store_object(path string, expected TccMacosLibgcFileHash) ! {
	stat, exists := lstat_tcc_macos_libgc_path(path)!
	if !exists {
		return error('macOS bundled libgc store object does not exist: ${path}')
	}
	validate_tcc_macos_libgc_store_object_stat(stat, os.geteuid())!
	actual := hash_tcc_macos_libgc_regular_file(path)!
	if actual.size != expected.size || actual.sum != expected.sum {
		return error('macOS bundled libgc store object failed its content hash validation')
	}
}

fn validate_tcc_macos_libgc_store_object_stat(stat os.Stat, expected_euid int) ! {
	if stat.get_filetype() != .regular {
		return error('macOS bundled libgc store object is not a regular file')
	}
	if stat.uid != u32(expected_euid) {
		return error('macOS bundled libgc store object is not owned by the effective user')
	}
	mode := stat.mode & 0o7777
	if mode != 0o700 {
		return error('macOS bundled libgc store object must have mode 0700, got ${mode:o}')
	}
	validate_tcc_macos_libgc_bounded_regular_stat(stat)!
}

fn tcc_macos_libgc_temp_directory_path(content_dir string, attempt int) string {
	temp_prefix := '.${tcc_macos_libgc_name}.tmp.${os.getpid()}.${sync.thread_id()}'
	return os.join_path(content_dir, '${temp_prefix}.${attempt}')
}

fn create_tcc_macos_libgc_exclusive_temp_directory(content_dir string) !string {
	for attempt in 0 .. tcc_macos_libgc_temp_attempts {
		temp_directory := tcc_macos_libgc_temp_directory_path(content_dir, attempt)
		os.mkdir(temp_directory, mode: 0o700) or {
			_, collision_exists := lstat_tcc_macos_libgc_path(temp_directory)!
			if collision_exists {
				continue
			}
			return error('cannot create exclusive macOS bundled libgc temporary directory ${temp_directory}: ${err.msg()}')
		}
		created_stat, created_exists := lstat_tcc_macos_libgc_path(temp_directory) or {
			inspection_error := err.msg()
			os.rmdir(temp_directory) or {
				return error('${inspection_error}; cannot clean unvalidated temporary directory ${temp_directory}: ${err.msg()}')
			}
			return error(inspection_error)
		}
		if !created_exists {
			return error('exclusive macOS bundled libgc temporary directory disappeared after creation: ${temp_directory}')
		}
		validate_tcc_macos_libgc_private_directory_stat(created_stat, os.geteuid()) or {
			validation_error := err.msg()
			os.rmdir(temp_directory) or {
				return error('${validation_error}; cannot clean invalid temporary directory ${temp_directory}: ${err.msg()}')
			}
			return error(validation_error)
		}
		return temp_directory
	}
	return error('cannot create exclusive macOS bundled libgc temporary directory after ${tcc_macos_libgc_temp_attempts} collisions')
}

fn stream_copy_tcc_macos_libgc(source string, destination string, expected TccMacosLibgcFileHash) ! {
	if expected.size == 0 || expected.size > tcc_macos_libgc_max_size {
		return error('macOS bundled libgc copy size ${expected.size} is outside the accepted range')
	}
	mut source_file := os.open(source)!
	defer {
		source_file.close()
	}
	_, destination_exists := lstat_tcc_macos_libgc_path(destination)!
	if destination_exists {
		return error('macOS bundled libgc temporary destination already exists: ${destination}')
	}
	mut destination_file := os.open_file(destination, 'wb', 0o600)!
	defer {
		destination_file.close()
	}
	destination_stat, destination_now_exists := lstat_tcc_macos_libgc_path(destination)!
	if !destination_now_exists || destination_stat.get_filetype() != .regular
		|| destination_stat.uid != u32(os.geteuid()) {
		return error('macOS bundled libgc temporary destination is not a regular euid-owned file')
	}
	mut buffer := []u8{len: tcc_macos_libgc_hash_buffer_size}
	mut total := u64(0)
	for {
		read := source_file.read(mut buffer) or {
			if err is os.Eof {
				break
			}
			return error('cannot read macOS bundled libgc source during streaming copy: ${err.msg()}')
		}
		if read <= 0 {
			break
		}
		total += u64(read)
		if total > expected.size || total > tcc_macos_libgc_max_size {
			return error('macOS bundled libgc source grew beyond the accepted copy size')
		}
		mut written := 0
		for written < read {
			count := destination_file.write(buffer[written..read]) or {
				return error('cannot write macOS bundled libgc temporary object: ${err.msg()}')
			}
			if count <= 0 {
				return error('cannot write macOS bundled libgc temporary object: zero-byte write')
			}
			written += count
		}
	}
	destination_file.close()
	source_file.close()
	if total != expected.size {
		return error('macOS bundled libgc source changed size during streaming copy: expected ${expected.size}, copied ${total}')
	}
}

fn copy_tcc_macos_libgc_to_exclusive_temp(source string, content_dir string, expected TccMacosLibgcFileHash) !TccMacosLibgcTemporaryObject {
	temp_directory := create_tcc_macos_libgc_exclusive_temp_directory(content_dir)!
	temporary := TccMacosLibgcTemporaryObject{
		directory: temp_directory
		path:      os.join_path(temp_directory, tcc_macos_libgc_name)
	}
	stream_copy_tcc_macos_libgc(source, temporary.path, expected) or {
		return tcc_macos_libgc_temporary_error(temporary,
			'cannot copy macOS bundled libgc to its exclusive temporary object: ${err.msg()}')
	}
	os.chmod(temporary.path, 0o700) or {
		return tcc_macos_libgc_temporary_error(temporary,
			'cannot set macOS bundled libgc temporary object mode: ${err.msg()}')
	}
	validate_tcc_macos_libgc_store_object(temporary.path, expected) or {
		return tcc_macos_libgc_temporary_error(temporary,
			'macOS bundled libgc temporary object failed post-close validation: ${err.msg()}')
	}
	return temporary
}

fn cleanup_tcc_macos_libgc_temporary(temporary TccMacosLibgcTemporaryObject) ! {
	directory_stat, directory_exists := lstat_tcc_macos_libgc_path(temporary.directory)!
	if !directory_exists {
		return error('macOS bundled libgc temporary directory disappeared before cleanup: ${temporary.directory}')
	}
	validate_tcc_macos_libgc_private_directory_stat(directory_stat, os.geteuid())!
	_, file_exists := lstat_tcc_macos_libgc_path(temporary.path)!
	if file_exists {
		os.rm(temporary.path) or {
			return error('cannot remove macOS bundled libgc temporary file ${temporary.path}: ${err.msg()}')
		}
	}
	os.rmdir(temporary.directory) or {
		return error('cannot remove macOS bundled libgc temporary directory ${temporary.directory}: ${err.msg()}')
	}
}

fn tcc_macos_libgc_temporary_error(temporary TccMacosLibgcTemporaryObject, operation_error string) IError {
	cleanup_tcc_macos_libgc_temporary(temporary) or {
		return error('${operation_error}; temporary cleanup also failed: ${err.msg()}')
	}
	return error(operation_error)
}

fn publish_tcc_macos_libgc_temporary(temporary TccMacosLibgcTemporaryObject, final_dylib string, expected TccMacosLibgcFileHash) !TccMacosLibgcPublicationResult {
	validate_tcc_macos_libgc_store_object(temporary.path, expected) or {
		return tcc_macos_libgc_temporary_error(temporary,
			'cannot publish invalid macOS bundled libgc temporary object: ${err.msg()}')
	}
	mut won := true
	os.link(temporary.path, final_dylib) or {
		won = false
		publication_error := err.msg()
		validate_tcc_macos_libgc_store_object(final_dylib, expected) or {
			return tcc_macos_libgc_temporary_error(temporary,
				'could not publish macOS bundled libgc atomically: ${publication_error}; concurrent object is invalid: ${err.msg()}')
		}
	}
	if won {
		validate_tcc_macos_libgc_store_object(final_dylib, expected) or {
			return tcc_macos_libgc_temporary_error(temporary,
				'published macOS bundled libgc object is invalid: ${err.msg()}')
		}
	}
	cleanup_tcc_macos_libgc_temporary(temporary) or {
		return error('macOS bundled libgc final object is valid, but temporary cleanup failed: ${err.msg()}')
	}
	return TccMacosLibgcPublicationResult{
		final_dylib: final_dylib
		won:         won
	}
}

fn lstat_tcc_macos_libgc_path(path string) !(os.Stat, bool) {
	stat := os.lstat(path) or {
		if err.code() == 2 {
			return os.Stat{}, false
		}
		return error('cannot inspect macOS bundled libgc path ${path}: ${err.msg()}')
	}
	return stat, true
}
