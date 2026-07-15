module modulecache

import os
import strings
import v3.flat
import v3.types

pub const builtin_bundle_imports = ['strconv', 'strings', 'hash', 'math.bits']
pub const builtin_bundle_modules = ['builtin', 'strconv', 'strings', 'hash', 'bits', 'math.bits']

const cache_format = 'v3-module-cache-29'
const c_body_begin = '/* V3CACHE_BODY_BEGIN */'
const c_body_end = '/* V3CACHE_BODY_END */'
const c_module_prefix = '/* V3CACHE_MODULE '
const source_body_marker = '// v3cache: source bodies required'

// Manager owns persistent v3 module cache paths for one compiler configuration.
pub struct Manager {
	build_pseudo_values string
pub:
	dir     string
	enabled bool
	salt    string
}

// Entry contains the persistent artifacts for one V module.
pub struct Entry {
pub:
	header       string
	object       string
	header_stamp string
	object_stamp string
	c_source     string
}

// CSplit contains the declaration/runtime prefix and per-module C function bodies.
pub struct CSplit {
pub:
	prefix  string
	modules map[string]string
}

// new_manager creates a configuration-scoped persistent module cache manager.
pub fn new_manager(vroot string, salt string, enabled bool, build_pseudo_values string) Manager {
	root_key := hash_text(os.real_path(vroot))
	config_key := hash_text(cache_format + '\n' + salt)
	base_dir := os.abs_path(os.getenv_opt('V3CACHE') or { os.vtmp_dir() })
	return Manager{
		dir:                 os.join_path(base_dir, 'v3_module_cache_${root_key}', config_key)
		enabled:             enabled
		salt:                salt
		build_pseudo_values: build_pseudo_values
	}
}

// ensure_dir creates the manager's private cache directory.
pub fn (m &Manager) ensure_dir() bool {
	if !m.enabled {
		return false
	}
	if !os.exists(m.dir) {
		os.mkdir_all(m.dir, mode: 0o700) or { return false }
	}
	return os.is_dir(m.dir) && os.is_readable(m.dir) && os.is_writable(m.dir)
}

// entry returns collision-resistant artifact paths for a module and source root.
pub fn (m &Manager) entry(module_name string, source_files []string) Entry {
	mut source_root := module_name
	if source_files.len > 0 {
		source_root = os.dir(os.real_path(source_files[0]))
	}
	id := '${sanitize_name(module_name)}_${hash_text(source_root)}'
	return Entry{
		header:       os.join_path(m.dir, '${id}.vh')
		object:       os.join_path(m.dir, '${id}.o')
		header_stamp: os.join_path(m.dir, '${id}.vh.stamp')
		object_stamp: os.join_path(m.dir, '${id}.body.stamp')
		c_source:     os.join_path(m.dir, '${id}.c')
	}
}

// object_entry returns the C artifacts for one effective compile-flag set.
pub fn (m &Manager) object_entry(module_name string, source_files []string, compile_signature string) Entry {
	entry := m.entry(module_name, source_files)
	key := hash_text(compile_signature)
	base := entry.object.all_before_last('.o')
	return Entry{
		header:       entry.header
		object:       '${base}_${key}.o'
		header_stamp: entry.header_stamp
		object_stamp: '${base}_${key}.o.stamp'
		c_source:     '${base}_${key}.c'
	}
}

// source_signature hashes selected source paths, contents, resolved module roots,
// build/environment values, and pkg-config probe results in stable order.
pub fn source_signature(source_files []string) string {
	return source_signature_with_build_values(source_files, '')
}

fn source_signature_with_build_values(source_files []string, build_pseudo_values string) string {
	mut files := source_files.clone()
	files.sort()
	mut hash := u64(1469598103934665603)
	mut env_names := map[string]bool{}
	mut pkgconfig_names := map[string]bool{}
	mut uses_build_pseudo := false
	for file in files {
		path := os.real_path(file)
		hash = hash_bytes(hash, path.bytes())
		hash = hash_bytes(hash, [u8(0)])
		content := os.read_bytes(file) or { return '' }
		hash = hash_bytes(hash, content)
		hash = hash_bytes(hash, [u8(0xff)])
		source := content.bytestr()
		if source.contains('@BUILD_TIMESTAMP') || source.contains('@BUILD_DATE')
			|| source.contains('@BUILD_TIME') {
			uses_build_pseudo = true
		}
		if source.contains('@VMODROOT') || source.contains('@VMOD_FILE')
			|| source.contains('@VROOT') {
			root, vmod_file := signature_vmod_root(file)
			hash = hash_bytes(hash, [u8(0xfc)])
			hash = hash_bytes(hash, root.bytes())
			hash = hash_bytes(hash, [u8(0)])
			if vmod_file.len > 0 {
				vmod_content := os.read_bytes(vmod_file) or { return '' }
				hash = hash_bytes(hash, [u8(1)])
				hash = hash_bytes(hash, vmod_file.bytes())
				hash = hash_bytes(hash, [u8(0)])
				hash = hash_bytes(hash, vmod_content)
			} else {
				hash = hash_bytes(hash, [u8(0)])
			}
			hash = hash_bytes(hash, [u8(0xff)])
		}
		for name in compile_time_env_names(source) {
			env_names[name] = true
		}
		for name in compile_time_pkgconfig_names(source) {
			pkgconfig_names[name] = true
		}
	}
	if uses_build_pseudo {
		hash = hash_bytes(hash, [u8(0xfb)])
		hash = hash_bytes(hash, build_pseudo_values.bytes())
		hash = hash_bytes(hash, [u8(0xff)])
	}
	mut names := env_names.keys()
	names.sort()
	for name in names {
		hash = hash_bytes(hash, [u8(0xfe)])
		hash = hash_bytes(hash, name.bytes())
		hash = hash_bytes(hash, [u8(0)])
		hash = hash_bytes(hash, os.getenv(name).bytes())
		hash = hash_bytes(hash, [u8(0xff)])
	}
	mut packages := pkgconfig_names.keys()
	packages.sort()
	for name in packages {
		available := os.execute('pkg-config --exists ${name}').exit_code == 0
		hash = hash_bytes(hash, [u8(0xfd)])
		hash = hash_bytes(hash, name.bytes())
		hash = hash_bytes(hash, [u8(0)])
		hash = hash_bytes(hash, [u8(if available { 1 } else { 0 })])
		hash = hash_bytes(hash, [u8(0xff)])
	}
	return hash.hex()
}

fn (m &Manager) source_signature(source_files []string) string {
	return source_signature_with_build_values(source_files, m.build_pseudo_values)
}

fn signature_vmod_root(source_file string) (string, string) {
	mut dir := os.dir(os.real_path(source_file))
	original_dir := dir
	for dir.len > 0 {
		vmod_file := os.join_path_single(dir, 'v.mod')
		if os.exists(vmod_file) {
			return os.real_path(dir), os.real_path(vmod_file)
		}
		parent := os.dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return os.real_path(original_dir), ''
}

fn compile_time_env_names(source string) []string {
	mut names := map[string]bool{}
	mut pos := 0
	for pos < source.len {
		if source[pos] == `/` && pos + 1 < source.len
			&& (source[pos + 1] == `/` || source[pos + 1] == `*`) {
			pos = skip_signature_space_and_comments(source, pos)
			continue
		}
		if source[pos] in [`'`, `"`, `\``] {
			pos = skip_signature_quoted_text(source, pos, false)
			continue
		}
		if source[pos] == `r` && pos + 1 < source.len && source[pos + 1] in [`'`, `"`] {
			pos = skip_signature_quoted_text(source, pos + 1, true)
			continue
		}
		if pos + 4 > source.len || source[pos..pos + 4] != '\$env' {
			pos++
			continue
		}
		mut arg_pos := skip_signature_space_and_comments(source, pos + 4)
		if arg_pos >= source.len || source[arg_pos] != `(` {
			pos += 4
			continue
		}
		arg_pos = skip_signature_space_and_comments(source, arg_pos + 1)
		mut is_raw := false
		if arg_pos + 1 < source.len && source[arg_pos] == `r` && source[arg_pos + 1] in [`'`, `"`] {
			is_raw = true
			arg_pos++
		}
		if arg_pos >= source.len || source[arg_pos] !in [`'`, `"`] {
			pos += 4
			continue
		}
		quote := source[arg_pos]
		name_start := arg_pos + 1
		mut name_end := name_start
		for name_end < source.len {
			if !is_raw && source[name_end] == `\\` && name_end + 1 < source.len {
				name_end += 2
				continue
			}
			if source[name_end] == quote {
				names[source[name_start..name_end]] = true
				break
			}
			name_end++
		}
		pos = if name_end < source.len { name_end + 1 } else { source.len }
	}
	mut result := names.keys()
	result.sort()
	return result
}

fn compile_time_pkgconfig_names(source string) []string {
	mut names := map[string]bool{}
	mut pos := 0
	for pos < source.len {
		if source[pos] == `/` && pos + 1 < source.len
			&& (source[pos + 1] == `/` || source[pos + 1] == `*`) {
			pos = skip_signature_space_and_comments(source, pos)
			continue
		}
		if source[pos] in [`'`, `"`, `\``] {
			pos = skip_signature_quoted_text(source, pos, false)
			continue
		}
		if source[pos] == `r` && pos + 1 < source.len && source[pos + 1] in [`'`, `"`] {
			pos = skip_signature_quoted_text(source, pos + 1, true)
			continue
		}
		if pos + 9 > source.len || source[pos..pos + 9] != 'pkgconfig'
			|| (pos > 0 && signature_name_char(source[pos - 1]))
			|| (pos + 9 < source.len && signature_name_char(source[pos + 9])) {
			pos++
			continue
		}
		name, next_pos, ok := signature_string_call_arg(source, pos + 9)
		if ok && signature_pkgconfig_name_is_safe(name) {
			names[name] = true
		}
		pos = if next_pos > pos { next_pos } else { pos + 9 }
	}
	mut result := names.keys()
	result.sort()
	return result
}

fn signature_string_call_arg(source string, start int) (string, int, bool) {
	mut pos := skip_signature_space_and_comments(source, start)
	if pos >= source.len || source[pos] != `(` {
		return '', start, false
	}
	pos = skip_signature_space_and_comments(source, pos + 1)
	mut is_raw := false
	if pos + 1 < source.len && source[pos] == `r` && source[pos + 1] in [`'`, `"`] {
		is_raw = true
		pos++
	}
	if pos >= source.len || source[pos] !in [`'`, `"`] {
		return '', start, false
	}
	quote := source[pos]
	value_start := pos + 1
	mut value_end := value_start
	for value_end < source.len {
		if !is_raw && source[value_end] == `\\` && value_end + 1 < source.len {
			value_end += 2
			continue
		}
		if source[value_end] == quote {
			return source[value_start..value_end], value_end + 1, true
		}
		value_end++
	}
	return '', source.len, false
}

fn signature_name_char(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`
}

fn signature_pkgconfig_name_is_safe(name string) bool {
	if name.len == 0 || !signature_name_char(name[0]) {
		return false
	}
	for c in name.bytes() {
		if signature_name_char(c) || c in [`-`, `.`, `+`] {
			continue
		}
		return false
	}
	return true
}

fn skip_signature_space_and_comments(source string, start int) int {
	mut pos := start
	for pos < source.len {
		for pos < source.len && source[pos] in [` `, `\t`, `\r`, `\n`] {
			pos++
		}
		if pos + 1 >= source.len || source[pos] != `/` {
			break
		}
		if source[pos + 1] == `/` {
			pos += 2
			for pos < source.len && source[pos] != `\n` {
				pos++
			}
			continue
		}
		if source[pos + 1] == `*` {
			pos += 2
			for pos + 1 < source.len && !(source[pos] == `*` && source[pos + 1] == `/`) {
				pos++
			}
			pos = if pos + 1 < source.len { pos + 2 } else { source.len }
			continue
		}
		break
	}
	return pos
}

fn skip_signature_quoted_text(source string, quote_pos int, is_raw bool) int {
	quote := source[quote_pos]
	mut pos := quote_pos + 1
	for pos < source.len {
		if !is_raw && source[pos] == `\\` && pos + 1 < source.len {
			pos += 2
			continue
		}
		if source[pos] == quote {
			return pos + 1
		}
		pos++
	}
	return source.len
}

// valid_entry reports whether both interface and object artifacts match their sources.
pub fn (m &Manager) valid_entry(module_name string, source_files []string) ?Entry {
	if !m.enabled || source_files.len == 0 {
		return none
	}
	entry := m.entry(module_name, source_files)
	if !os.is_file(entry.header) || !os.is_file(entry.header_stamp)
		|| !os.is_file(entry.object_stamp) {
		return none
	}
	stamp := os.read_file(entry.header_stamp) or { return none }
	expected := entry_stamp(m.salt, m.source_signature(source_files))
	if stamp != expected {
		return none
	}
	object_stamp := os.read_file(entry.object_stamp) or { return none }
	if !object_stamp_valid(object_stamp, expected) {
		return none
	}
	return entry
}

// valid_header reports whether a declaration header matches its module sources.
pub fn (m &Manager) valid_header(module_name string, source_files []string) ?Entry {
	if !m.enabled || source_files.len == 0 {
		return none
	}
	entry := m.entry(module_name, source_files)
	if !os.is_file(entry.header) || !os.is_file(entry.header_stamp) {
		return none
	}
	stamp := os.read_file(entry.header_stamp) or { return none }
	if stamp != entry_stamp(m.salt, m.source_signature(source_files)) {
		return none
	}
	return entry
}

// header_needs_source reports whether a declaration header has bodies that must
// remain available to per-program monomorphization.
pub fn header_needs_source(entry Entry) bool {
	header := os.read_file(entry.header) or { return true }
	return header.contains(source_body_marker)
}

// valid_object reports whether a cached object matches the supplied sources.
pub fn (m &Manager) valid_object(cache_name string, source_files []string) ?Entry {
	if !m.enabled || source_files.len == 0 {
		return none
	}
	entry := m.entry(cache_name, source_files)
	if !os.is_file(entry.object_stamp) {
		return none
	}
	stamp := os.read_file(entry.object_stamp) or { return none }
	if !object_stamp_valid(stamp, entry_stamp(m.salt, m.source_signature(source_files))) {
		return none
	}
	return entry
}

// write_entry commits a module interface and stamp after its object was built.
pub fn (m &Manager) write_entry(module_name string, source_files []string, header string) !Entry {
	if !m.ensure_dir() {
		return error('v3 module cache directory is unavailable')
	}
	entry := m.entry(module_name, source_files)
	write_atomic(entry.header, header)!
	write_atomic(entry.header_stamp, entry_stamp(m.salt, m.source_signature(source_files)))!
	return entry
}

// write_header commits one declaration-only module header and its source stamp.
pub fn (m &Manager) write_header(module_name string, source_files []string, header string) !Entry {
	if !m.ensure_dir() {
		return error('v3 module cache directory is unavailable')
	}
	entry := m.entry(module_name, source_files)
	write_atomic(entry.header, header)!
	write_atomic(entry.header_stamp, entry_stamp(m.salt, m.source_signature(source_files)))!
	return entry
}

// valid_object_for_compile_signature reports whether the flag-specific object
// matches its sources, dependency headers, and effective C compilation flags.
pub fn (m &Manager) valid_object_for_compile_signature(cache_name string, source_files []string, compile_signature string, dependency_inputs map[string]string) ?Entry {
	entry := m.object_entry(cache_name, source_files, compile_signature)
	if !os.is_file(entry.object) || !os.is_file(entry.object_stamp) {
		return none
	}
	stamp := os.read_file(entry.object_stamp) or { return none }
	if !object_stamp_valid(stamp, entry_stamp(m.salt, m.source_signature(source_files))) {
		return none
	}
	expected := 'compile=${hash_text(compile_signature)}'
	if !stamp.split_into_lines().any(it == expected) {
		return none
	}
	if !object_stamp_dependencies_match(stamp, dependency_inputs) {
		return none
	}
	return entry
}

// write_stamp refreshes a cache stamp after the object and header are durable.
// dependency_inputs maps every transitive imported `.vh` and non-V input path
// to the content signature that the object was compiled against.
pub fn (m &Manager) write_stamp(module_name string, source_files []string, dependency_inputs map[string]string, compile_signature string) ! {
	entry := m.entry(module_name, source_files)
	object_entry := m.object_entry(module_name, source_files, compile_signature)
	stamp := object_entry_stamp(m.salt, m.source_signature(source_files), dependency_inputs,
		compile_signature)
	write_atomic(object_entry.object_stamp, stamp)!
	write_atomic(entry.object_stamp, stamp)!
}

fn write_atomic(path string, content string) ! {
	tmp := '${path}.tmp.${os.getpid()}'
	defer {
		os.rm(tmp) or {}
	}
	os.write_file(tmp, content)!
	os.mv(tmp, path)!
}

fn entry_stamp(salt string, source_hash string) string {
	return 'format=${cache_format}\nconfig=${hash_text(salt)}\nsource=${source_hash}\n'
}

fn object_entry_stamp(salt string, source_hash string, dependency_inputs map[string]string, compile_signature string) string {
	mut out := strings.new_builder(256 + dependency_inputs.len * 96)
	out.write_string(entry_stamp(salt, source_hash))
	out.writeln('compile=${hash_text(compile_signature)}')
	mut paths := dependency_inputs.keys()
	paths.sort()
	for path in paths {
		out.writeln('dependency=${path}\t${dependency_inputs[path]}')
	}
	return out.str()
}

fn object_stamp_valid(stamp string, expected_entry string) bool {
	if !stamp.starts_with(expected_entry) {
		return false
	}
	mut has_compile_signature := false
	for line in stamp[expected_entry.len..].split_into_lines() {
		if line.len == 0 {
			continue
		}
		if line.starts_with('compile=') {
			if has_compile_signature || line.len == 'compile='.len {
				return false
			}
			has_compile_signature = true
			continue
		}
		if !line.starts_with('dependency=') {
			return false
		}
		value := line['dependency='.len..]
		tab := value.last_index_u8(`\t`)
		if tab <= 0 || tab + 1 >= value.len {
			return false
		}
		path := value[..tab]
		expected_signature := value[tab + 1..]
		if file_signature(path) != expected_signature {
			return false
		}
	}
	return has_compile_signature
}

fn object_stamp_dependencies_match(stamp string, expected map[string]string) bool {
	mut actual := map[string]string{}
	for line in stamp.split_into_lines() {
		if !line.starts_with('dependency=') {
			continue
		}
		value := line['dependency='.len..]
		tab := value.last_index_u8(`\t`)
		if tab <= 0 || tab + 1 >= value.len {
			return false
		}
		path := value[..tab]
		if path in actual {
			return false
		}
		actual[path] = value[tab + 1..]
	}
	if actual.len != expected.len {
		return false
	}
	for path, signature in expected {
		if actual[path] != signature {
			return false
		}
	}
	return true
}

// header_signature returns the stable content signature stored in dependent
// object stamps for a declaration-only module header.
pub fn header_signature(header string) string {
	return hash_text(header)
}

// file_signature returns a stable content signature for a cache input file.
pub fn file_signature(path string) string {
	content := os.read_bytes(path) or { return '' }
	return hash_bytes(u64(1469598103934665603), content).hex()
}

fn sanitize_name(name string) string {
	mut out := strings.new_builder(name.len + 8)
	for c in name.bytes() {
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`)
			|| c == `_` || c == `-` {
			out.write_u8(c)
		} else {
			out.write_u8(`_`)
		}
	}
	return out.str()
}

fn hash_text(value string) string {
	return hash_bytes(u64(1469598103934665603), value.bytes()).hex()
}

fn hash_bytes(initial u64, bytes []u8) u64 {
	mut hash := initial
	for c in bytes {
		hash = (hash ^ u64(c)) * u64(1099511628211)
	}
	return hash
}

// split_generated_c separates a cache-marked monolithic translation unit.
pub fn split_generated_c(source string) !CSplit {
	begin_line := '${c_body_begin}\n'
	mut begin := 0
	if !source.starts_with(begin_line) {
		begin = source.index('\n${begin_line}') or { return error('missing v3 cache body marker') }
		begin++
	}
	body_start := begin + begin_line.len
	end := source.index_after('\n${c_body_end}', body_start) or {
		return error('missing v3 cache body end marker')
	}
	prefix := source[..begin]
	body := source[body_start..end]
	mut module_segments := map[string][]string{}
	mut current := ''
	mut segment_start := 0
	mut pos := 0
	for {
		marker_start := body.index_after(c_module_prefix, pos) or { break }
		if marker_start > 0 && body[marker_start - 1] != `\n` {
			pos = marker_start + c_module_prefix.len
			continue
		}
		line_end := body.index_after('\n', marker_start) or { body.len }
		marker_line := body[marker_start..line_end]
		if !marker_line.ends_with(' */') {
			pos = if line_end < body.len { line_end + 1 } else { body.len }
			continue
		}
		if current.len > 0 {
			module_segments[current] << body[segment_start..marker_start]
		}
		current = marker_line[c_module_prefix.len..marker_line.len - 3].trim_space()
		segment_start = line_end
		pos = if line_end < body.len { line_end + 1 } else { body.len }
	}
	if current.len > 0 && segment_start < body.len {
		module_segments[current] << body[segment_start..]
	}
	mut modules := map[string]string{}
	for name, segments in module_segments {
		modules[name] = segments.join('')
	}
	return CSplit{
		prefix:  prefix
		modules: modules
	}
}

// declaration_header converts the generated owner prefix into a C header for
// cached module translation units. Type definitions and static helpers stay
// local; owner functions and storage become declarations resolved from main.o.
pub fn declaration_header(prefix string) string {
	header, _ := c_declaration_header(prefix)
	return header
}

// c_source_has_static_storage reports whether a local C input would give cached
// translation units separate copies of static storage.
pub fn c_source_has_static_storage(source string) bool {
	_, has_static_storage := c_declaration_header(source)
	return has_static_storage
}

fn c_declaration_header(prefix string) (string, bool) {
	mut out := strings.new_builder(prefix.len / 2)
	mut item := strings.new_builder(512)
	mut brace_depth := 0
	mut has_brace := false
	mut has_static_storage := false
	mut in_block_comment := false
	mut in_preprocessor_directive := false
	for raw_line in prefix.split_into_lines() {
		line := raw_line + '\n'
		trimmed := raw_line.trim_space()
		if in_preprocessor_directive {
			out.write_string(line)
			in_preprocessor_directive = c_preprocessor_line_continues(raw_line)
			continue
		}
		if brace_depth == 0 && trimmed.starts_with('#') && item.len > 0 {
			pending := item.str()
			item = strings.new_builder(512)
			if trim_leading_c_comments(pending).len == 0 {
				out.write_string(pending)
				has_brace = false
			} else {
				item.write_string(pending)
			}
		}
		if brace_depth == 0 && item.len == 0
			&& (trimmed.starts_with('#') || trimmed.len == 0 || trimmed.starts_with('//')) {
			out.write_string(line)
			if trimmed.starts_with('#') {
				in_preprocessor_directive = c_preprocessor_line_continues(raw_line)
			}
			continue
		}
		item.write_string(line)
		delta, saw_brace, next_comment := c_line_braces(raw_line, in_block_comment)
		in_block_comment = next_comment
		brace_depth += delta
		has_brace = has_brace || saw_brace
		if brace_depth > 0 {
			continue
		}
		if has_brace {
			// Function and type blocks emitted by v3 finish at depth zero. A
			// typedef/global initializer may carry its semicolon on the same line.
			if !trimmed.ends_with('}') && !trimmed.ends_with(';') {
				continue
			}
		} else if !trimmed.ends_with(';') {
			continue
		}
		declaration := item.str()
		has_static_storage = has_static_storage
			|| c_declaration_item_has_static_storage(declaration, has_brace)
		out.write_string(c_declaration_item(declaration, has_brace))
		item = strings.new_builder(512)
		has_brace = false
	}
	if item.len > 0 {
		declaration := item.str()
		has_static_storage = has_static_storage
			|| c_declaration_item_has_static_storage(declaration, has_brace)
		out.write_string(c_declaration_item(declaration, has_brace))
	}
	return out.str(), has_static_storage
}

fn c_preprocessor_line_continues(line string) bool {
	return line.trim_right('\r').ends_with('\\')
}

fn c_declaration_item(item string, has_brace bool) string {
	trimmed := item.trim_space()
	if trimmed.len == 0 {
		return item
	}
	clean := trim_leading_c_comments(trimmed)
	if block := c_extern_c_block(item) {
		inner_header, _ := c_declaration_header(block.inner)
		mut result := block.before
		if !result.ends_with('\n') && !inner_header.starts_with('\n') {
			result += '\n'
		}
		result += inner_header
		if !result.ends_with('\n') && !block.after.starts_with('\n') {
			result += '\n'
		}
		return result + block.after
	}
	brace := if has_brace { clean.index_u8(`{`) } else { -1 }
	head := if brace > 0 { clean[..brace].trim_space() } else { clean }
	if c_has_static_storage_class(head) || clean.starts_with('typedef ')
		|| c_tag_declaration_is_type_only(clean, has_brace) {
		return item
	}
	if has_brace && brace > 0 {
		if c_declaration_head_is_function(head) {
			if c_declaration_head_keeps_definition(head) {
				return item
			}
			return '${head};\n'
		}
		if c_tag_declaration_keyword_len(clean) > 0 {
			return c_extern_storage_decl(clean.trim_right(';'))
		}
		return c_extern_storage_decl(head)
	}
	if clean.starts_with('extern ') || clean.starts_with('_Static_assert')
		|| c_declaration_head_is_function(clean) {
		return item
	}
	return c_extern_storage_decl(clean.trim_right(';'))
}

fn c_declaration_item_has_static_storage(item string, has_brace bool) bool {
	clean := trim_leading_c_comments(item.trim_space())
	if block := c_extern_c_block(item) {
		_, has_static_storage := c_declaration_header(block.inner)
		return has_static_storage
	}
	mut storage_head := clean
	if has_brace {
		brace := clean.index_u8(`{`)
		if brace > 0 {
			head := clean[..brace].trim_space()
			storage_head = head
			if c_declaration_head_is_function(head) {
				return c_declaration_head_keeps_definition(head)
					&& c_code_contains_identifier(clean[brace + 1..], 'static')
			}
		}
	}
	if !c_has_static_storage_class(storage_head) {
		return false
	}
	if !has_brace {
		return !c_declaration_head_is_function(clean.trim_right(';'))
	}
	return true
}

struct CExternBlock {
	before string
	inner  string
	after  string
}

fn c_extern_c_block(item string) ?CExternBlock {
	clean := trim_leading_c_comments(item.trim_space())
	if !clean.starts_with('extern "C"') {
		return none
	}
	rest := clean['extern "C"'.len..].trim_space()
	if !rest.starts_with('{') {
		return none
	}
	marker_pos := item.index('extern "C"') or { return none }
	brace_offset := item[marker_pos + 'extern "C"'.len..].index_u8(`{`)
	if brace_offset < 0 {
		return none
	}
	open := marker_pos + 'extern "C"'.len + brace_offset
	close := item.last_index_u8(`}`)
	if close <= open {
		return none
	}
	return CExternBlock{
		before: item[..open + 1]
		inner:  item[open + 1..close]
		after:  item[close..]
	}
}

fn c_has_static_storage_class(value string) bool {
	return c_code_contains_identifier(value, 'static')
}

fn c_declaration_head_is_function(value string) bool {
	return value.contains('(') && !c_contains_parenthesized_pointer_declarator(value)
		&& !c_contains_declaration_attribute(value) && !c_has_top_level_assign(value)
}

fn c_declaration_head_keeps_definition(value string) bool {
	if c_has_static_storage_class(value) {
		return true
	}
	return !c_code_contains_identifier(value, 'extern')
		&& (c_code_contains_identifier(value, 'inline')
		|| c_code_contains_identifier(value, '__inline')
		|| c_code_contains_identifier(value, '__inline__'))
}

fn c_code_contains_identifier(value string, name string) bool {
	if name.len == 0 {
		return false
	}
	mut quote := u8(0)
	mut escaped := false
	mut block_comment := false
	mut line_comment := false
	for i, c in value.bytes() {
		if quote != 0 {
			if escaped {
				escaped = false
			} else if c == `\\` {
				escaped = true
			} else if c == quote {
				quote = 0
			}
			continue
		}
		next := if i + 1 < value.len { value[i + 1] } else { u8(0) }
		if block_comment {
			if c == `*` && next == `/` {
				block_comment = false
			}
			continue
		}
		if line_comment {
			if c == `\n` {
				line_comment = false
			}
			continue
		}
		if c == `/` && next == `*` {
			block_comment = true
			continue
		}
		if c == `/` && next == `/` {
			line_comment = true
			continue
		}
		if c == `'` || c == `"` {
			quote = c
			continue
		}
		if c != name[0] || i + name.len > value.len || value[i..i + name.len] != name {
			continue
		}
		prev := if i > 0 { value[i - 1] } else { u8(0) }
		after := if i + name.len < value.len { value[i + name.len] } else { u8(0) }
		if !signature_name_char(prev) && !signature_name_char(after) {
			return true
		}
	}
	return false
}

fn c_contains_parenthesized_pointer_declarator(value string) bool {
	for i, c in value.bytes() {
		if c != `(` {
			continue
		}
		mut pos := i + 1
		for pos < value.len && value[pos] in [` `, `\t`, `\r`, `\n`] {
			pos++
		}
		if pos < value.len && value[pos] == `*` {
			return true
		}
	}
	return false
}

fn c_contains_declaration_attribute(value string) bool {
	return value.contains('__attribute__') || value.contains('__declspec')
		|| value.contains('_Alignas(') || value.contains('alignas(')
}

fn c_tag_declaration_is_type_only(value string, has_brace bool) bool {
	keyword_len := c_tag_declaration_keyword_len(value)
	if keyword_len == 0 {
		return false
	}
	if !has_brace {
		head := value.trim_right(';').trim_space()
		return c_tag_definition_head_is_type_only(head, keyword_len)
	}
	open := value.index_u8(`{`)
	close := value.last_index_u8(`}`)
	if open < 0 || close <= open {
		return false
	}
	head := value[..open].trim_space()
	if c_has_top_level_assign(head) || !c_tag_definition_head_is_type_only(head, keyword_len) {
		return false
	}
	tail := value[close + 1..].trim_space().trim_right(';').trim_space()
	return tail.len == 0
		|| ((tail.starts_with('__attribute__') || tail.starts_with('__declspec'))
		&& tail.ends_with(')'))
}

fn c_tag_declaration_keyword_len(value string) int {
	for keyword in ['struct', 'union', 'enum'] {
		if value.len > keyword.len && value[..keyword.len] == keyword
			&& value[keyword.len] in [` `, `\t`, `\r`, `\n`] {
			return keyword.len
		}
	}
	return 0
}

fn c_tag_definition_head_is_type_only(head string, keyword_len int) bool {
	mut tail := head[keyword_len..].trim_space()
	if tail.len == 0 {
		return true
	}
	if tail.starts_with('__attribute__') || tail.starts_with('__declspec') {
		return true
	}
	mut name_end := 0
	for name_end < tail.len && (signature_name_char(tail[name_end]) || tail[name_end] == `$`) {
		name_end++
	}
	if name_end == 0 {
		return false
	}
	tail = tail[name_end..].trim_space()
	return tail.len == 0 || tail.starts_with(':') || tail.starts_with('__attribute__')
		|| tail.starts_with('__declspec')
}

fn c_extern_storage_decl(head string) string {
	mut declarators := []string{}
	for part in c_split_top_level_declarators(head.trim_space()) {
		mut declaration := part.trim_space()
		if eq := c_top_level_assign_index(declaration) {
			declaration = declaration[..eq].trim_space()
		}
		if declaration.len > 0 {
			declarators << declaration
		}
	}
	if declarators.len == 0 {
		return ''
	}
	return 'extern ${declarators.join(', ')};\n'
}

fn c_split_top_level_declarators(value string) []string {
	mut parts := []string{}
	mut start := 0
	mut paren := 0
	mut bracket := 0
	mut brace := 0
	mut quote := u8(0)
	mut escaped := false
	mut block_comment := false
	mut line_comment := false
	for i, c in value.bytes() {
		if quote != 0 {
			if escaped {
				escaped = false
			} else if c == `\\` {
				escaped = true
			} else if c == quote {
				quote = 0
			}
			continue
		}
		next := if i + 1 < value.len { value[i + 1] } else { u8(0) }
		if block_comment {
			if c == `*` && next == `/` {
				block_comment = false
			}
			continue
		}
		if line_comment {
			if c == `\n` {
				line_comment = false
			}
			continue
		}
		if c == `/` && next == `*` {
			block_comment = true
			continue
		}
		if c == `/` && next == `/` {
			line_comment = true
			continue
		}
		if c == `'` || c == `"` {
			quote = c
			continue
		}
		match c {
			`(` {
				paren++
			}
			`)` {
				paren--
			}
			`[` {
				bracket++
			}
			`]` {
				bracket--
			}
			`{` {
				brace++
			}
			`}` {
				brace--
			}
			`,` {
				if paren == 0 && bracket == 0 && brace == 0 {
					parts << value[start..i]
					start = i + 1
				}
			}
			else {}
		}
	}
	parts << value[start..]
	return parts
}

fn c_has_top_level_assign(value string) bool {
	return c_top_level_assign_index(value) != none
}

fn c_top_level_assign_index(value string) ?int {
	mut paren := 0
	mut bracket := 0
	mut brace := 0
	mut quote := u8(0)
	mut escaped := false
	mut block_comment := false
	mut line_comment := false
	for i, c in value.bytes() {
		if quote != 0 {
			if escaped {
				escaped = false
			} else if c == `\\` {
				escaped = true
			} else if c == quote {
				quote = 0
			}
			continue
		}
		next := if i + 1 < value.len { value[i + 1] } else { u8(0) }
		if block_comment {
			if c == `*` && next == `/` {
				block_comment = false
			}
			continue
		}
		if line_comment {
			if c == `\n` {
				line_comment = false
			}
			continue
		}
		if c == `/` && next == `*` {
			block_comment = true
			continue
		}
		if c == `/` && next == `/` {
			line_comment = true
			continue
		}
		if c == `'` || c == `"` {
			quote = c
			continue
		}
		match c {
			`(` {
				paren++
			}
			`)` {
				paren--
			}
			`[` {
				bracket++
			}
			`]` {
				bracket--
			}
			`{` {
				brace++
			}
			`}` {
				brace--
			}
			`=` {
				if paren == 0 && bracket == 0 && brace == 0 {
					prev := if i > 0 { value[i - 1] } else { u8(0) }
					if prev !in [`=`, `!`, `<`, `>`] && next != `=` {
						return i
					}
				}
			}
			else {}
		}
	}
	return none
}

fn trim_leading_c_comments(value string) string {
	mut clean := value.trim_space()
	for {
		if clean.starts_with('//') {
			newline := clean.index_u8(`\n`)
			if newline < 0 {
				return ''
			}
			clean = clean[newline + 1..].trim_space()
			continue
		}
		if clean.starts_with('/*') {
			end := clean.index('*/') or { return '' }
			clean = clean[end + 2..].trim_space()
			continue
		}
		return clean
	}
	return clean
}

fn c_line_braces(line string, initial_block_comment bool) (int, bool, bool) {
	mut depth := 0
	mut saw := false
	mut quote := u8(0)
	mut escaped := false
	mut block_comment := initial_block_comment
	mut i := 0
	bytes := line.bytes()
	for i < bytes.len {
		c := bytes[i]
		next := if i + 1 < bytes.len { bytes[i + 1] } else { u8(0) }
		if block_comment {
			if c == `*` && next == `/` {
				block_comment = false
				i += 2
				continue
			}
			i++
			continue
		}
		if quote != 0 {
			if escaped {
				escaped = false
			} else if c == `\\` {
				escaped = true
			} else if c == quote {
				quote = 0
			}
			i++
			continue
		}
		if c == `/` && next == `/` {
			break
		}
		if c == `/` && next == `*` {
			block_comment = true
			i += 2
			continue
		}
		if c == `'` || c == `"` {
			quote = c
			i++
			continue
		}
		if c == `{` {
			depth++
			saw = true
		} else if c == `}` {
			depth--
			saw = true
		}
		i++
	}
	return depth, saw, block_comment
}

// module_header serializes the declaration-only interface for one flat-AST module.
pub fn module_header(a &flat.FlatAst, tc &types.TypeChecker, module_name string, vroot string, import_paths map[string]string) string {
	mut out := strings.new_builder(4096)
	out.writeln('module ${module_name.all_after_last('.')}')
	if module_needs_source_bodies(a, tc, module_name) {
		out.writeln(source_body_marker)
	}
	out.writeln('')
	mut seen := map[string]bool{}
	declaration_attrs := cached_declaration_attrs(a)
	for file_node in a.nodes {
		if file_node.kind != .file || file_node.children_count == 0 {
			continue
		}
		file_module := file_module_name(a, file_node)
		if file_module != module_name {
			continue
		}
		for i in 0 .. file_node.children_count {
			mut decl_ids := []flat.NodeId{}
			append_declaration_nodes(a, a.child(&file_node, i), mut decl_ids)
			for id in decl_ids {
				node := a.nodes[int(id)]
				if node.kind == .module_decl {
					continue
				}
				key := decl_key(a, node)
				if key.len > 0 && seen[key] {
					continue
				}
				attrs := declaration_attrs[int(id)] or { CachedDeclarationAttrs{} }
				mut text := decl_text(a, tc, module_name, node, vroot, file_node.value,
					import_paths, attrs.attrs)
				if text.len == 0 {
					continue
				}
				attrs_text := cached_declaration_attrs_text(attrs)
				if attrs_text.len > 0 {
					text = '${attrs_text}\n${text}'
				}
				if key.len > 0 {
					seen[key] = true
				}
				out.writeln(text)
				out.writeln('')
			}
		}
	}
	// Implicit imports are spliced into the flat top-level stream after parsing
	// (`sync` for locks and `v.embed_file` for `$embed_file`). They are not file
	// children, so preserve them separately or a warm header would omit the
	// cached object's link dependencies.
	mut stream_module := ''
	for node in a.nodes {
		if node.kind == .file {
			stream_module = file_module_name(a, node)
			continue
		}
		if node.kind != .import_decl || stream_module != module_name {
			continue
		}
		key := decl_key(a, node)
		if seen[key] {
			continue
		}
		text := import_text(a, node, import_paths)
		if text.len == 0 {
			continue
		}
		seen[key] = true
		out.writeln(text)
		out.writeln('')
	}
	return out.str()
}

struct CachedDeclarationAttrs {
	attrs []string
	kinds []int
}

fn cached_declaration_attrs(a &flat.FlatAst) map[int]CachedDeclarationAttrs {
	mut result := map[int]CachedDeclarationAttrs{}
	for node in a.nodes {
		if node.kind != .directive || !node.value.starts_with('@attributes:') {
			continue
		}
		decl_id := node.value['@attributes:'.len..].int()
		if decl_id < 0 || decl_id >= a.nodes.len || node.generic_params.len == 0 {
			continue
		}
		result[decl_id] = CachedDeclarationAttrs{
			attrs: node.generic_params.clone()
			kinds: if node.typ.len > 0 {
				node.typ.split(',').map(it.int())
			} else {
				[]int{}
			}
		}
	}
	return result
}

fn cached_declaration_attrs_text(data CachedDeclarationAttrs) string {
	mut lines := []string{cap: data.attrs.len}
	for i, raw_attr in data.attrs {
		attr := cached_declaration_attr_source(raw_attr, if i < data.kinds.len {
			data.kinds[i]
		} else {
			0
		})
		if attr.len > 0 {
			lines << '@[${attr}]'
		}
	}
	return lines.join('\n')
}

fn cached_declaration_attr_source(raw_attr string, kind int) string {
	attr := raw_attr.trim_space()
	if attr.len == 0 {
		return ''
	}
	if kind == 1 {
		colon := attr.index_u8(`:`)
		if colon < 0 || !cached_attribute_string_literal(attr[colon + 1..].trim_space()) {
			return "'${escape_v_string(attr)}'"
		}
	}
	return attr
}

fn cached_attribute_string_literal(value string) bool {
	return value.len >= 2 && value[0] in [`'`, `\"`] && value[value.len - 1] == value[0]
}

fn cached_declaration_has_attr(attrs []string, name string) bool {
	for attr in attrs {
		if attr.all_before(':').trim_space() == name {
			return true
		}
	}
	return false
}

fn append_declaration_nodes(a &flat.FlatAst, id flat.NodeId, mut declarations []flat.NodeId) {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return
	}
	node := a.nodes[int(id)]
	if node.kind == .block {
		for i in 0 .. node.children_count {
			append_declaration_nodes(a, a.child(&node, i), mut declarations)
		}
		return
	}
	declarations << id
}

fn module_needs_source_bodies(a &flat.FlatAst, tc &types.TypeChecker, module_name string) bool {
	for file_node in a.nodes {
		if file_node.kind != .file || file_node.children_count == 0 {
			continue
		}
		file_module := file_module_name(a, file_node)
		if file_module != module_name {
			continue
		}
		for i in 0 .. file_node.children_count {
			id := a.child(&file_node, i)
			if declaration_node_needs_source(a, id)
				|| node_creates_generic_specialization(a, tc, id) {
				return true
			}
		}
	}
	return false
}

fn node_creates_generic_specialization(a &flat.FlatAst, tc &types.TypeChecker, id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return false
	}
	if name := tc.resolved_call_name(id) {
		if name in tc.fn_generic_params {
			return true
		}
	}
	if name := tc.resolved_fn_value_name(id) {
		if name in tc.fn_generic_params {
			return true
		}
	}
	node := a.nodes[int(id)]
	for i in 0 .. node.children_count {
		if node_creates_generic_specialization(a, tc, a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn declaration_node_needs_source(a &flat.FlatAst, id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return false
	}
	node := a.nodes[int(id)]
	if node.generic_params.len > 0 || fn_decl_has_generic_receiver(a, node)
		|| (node.kind in [.const_decl, .struct_decl, .global_decl]
		&& declaration_has_unserializable_initializer(a, node))
		|| node.kind == .comptime_if
		|| (node.kind == .struct_decl && struct_has_unserializable_children(a, node)) {
		return true
	}
	if node.kind == .block {
		for i in 0 .. node.children_count {
			if declaration_node_needs_source(a, a.child(&node, i)) {
				return true
			}
		}
	}
	return false
}

fn struct_has_unserializable_children(a &flat.FlatAst, node flat.Node) bool {
	for i in 0 .. node.children_count {
		child := a.child_node(&node, i)
		if child.kind == .field_decl {
			continue
		}
		if child.kind == .block {
			if struct_has_unserializable_children(a, *child) {
				return true
			}
			continue
		}
		if child.kind != .empty {
			return true
		}
	}
	return false
}

fn declaration_has_unserializable_initializer(a &flat.FlatAst, node flat.Node) bool {
	for i in 0 .. node.children_count {
		field := a.child_node(&node, i)
		if field.kind == .block {
			if declaration_has_unserializable_initializer(a, *field) {
				return true
			}
			continue
		}
		if field.children_count > 0 && !expr_can_serialize(a, a.child(field, 0)) {
			return true
		}
	}
	return false
}

fn fn_decl_has_generic_receiver(a &flat.FlatAst, node flat.Node) bool {
	if node.kind != .fn_decl || !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	receiver := a.child_node(&node, 0)
	if receiver.kind != .param {
		return false
	}
	receiver_type := clean_receiver_type(receiver.typ)
	declared_receiver := node.value.all_before_last('.')
	if receiver_type != declared_receiver
		&& receiver_type.all_after_last('.') != declared_receiver.all_after_last('.') {
		return false
	}
	return receiver_type.contains('[') && receiver_type.contains(']')
}

fn file_module_name(a &flat.FlatAst, file_node flat.Node) string {
	for i in 0 .. file_node.children_count {
		child := a.child_node(&file_node, i)
		if child.kind == .module_decl {
			return child.value
		}
	}
	return ''
}

fn decl_key(a &flat.FlatAst, node flat.Node) string {
	return match node.kind {
		.import_decl {
			mut selected := []string{}
			for i in 0 .. node.children_count {
				selected << a.child_node(&node, i).value
			}
			'import:${node.value}:${node.typ}:${selected.join(',')}'
		}
		.fn_decl, .c_fn_decl, .struct_decl, .enum_decl, .type_decl, .interface_decl {
			'${int(node.kind)}:${node.value}'
		}
		.directive {
			// C preprocessor directives are stateful and order-sensitive. Do not
			// deduplicate repeated branches, includes, or matching `#endif`s.
			''
		}
		else {
			''
		}
	}
}

fn decl_text(a &flat.FlatAst, tc &types.TypeChecker, module_name string, node flat.Node, vroot string, source_file string, import_paths map[string]string, declaration_attrs []string) string {
	return match node.kind {
		.import_decl {
			import_text(a, node, import_paths)
		}
		.fn_decl {
			fn_text(a, module_name, node, false, declaration_attrs)
		}
		.c_fn_decl {
			fn_text(a, module_name, node, true, declaration_attrs)
		}
		.struct_decl {
			struct_text(a, node, declaration_attrs)
		}
		.global_decl {
			global_text(a, tc, module_name, node)
		}
		.const_decl {
			const_text(a, node)
		}
		.enum_decl {
			enum_text(a, node, declaration_attrs)
		}
		.type_decl {
			type_text(a, node)
		}
		.interface_decl {
			interface_text(a, node)
		}
		.directive {
			cached_directive_text(node, vroot, source_file)
		}
		else {
			''
		}
	}
}

fn cached_directive_text(node flat.Node, vroot string, source_file string) string {
	if node.value !in ['include', 'insert', 'flag', 'pkgconfig', 'define', 'undef', 'ifdef', 'ifndef',
		'if', 'elif', 'else', 'endif', 'pragma', 'error', 'warning'] {
		return ''
	}
	value := cached_directive_value(node.value, node.typ, vroot, source_file)
	return if value.len > 0 { '#${node.value} ${value}' } else { '#${node.value}' }
}

fn cached_directive_value(directive string, value string, vroot string, source_file string) string {
	mut result := value.replace('@VEXEROOT', vroot)
	result = result.replace('@VROOT', '@VMODROOT')
	if result.contains('@VMODROOT') {
		result = result.replace('@VMODROOT', cached_vmod_root(source_file))
	}
	if result.contains('@DIR') {
		result = result.replace('@DIR', os.real_path(os.dir(source_file)))
	}
	if directive in ['include', 'insert'] {
		return cached_resolve_relative_include_path(result, source_file)
	}
	if directive == 'flag' {
		return cached_resolve_relative_flag_paths(result, source_file)
	}
	return result
}

fn cached_resolve_relative_include_path(value string, source_file string) string {
	if source_file.len == 0 {
		return value
	}
	quote_start := value.index_u8(`"`)
	if quote_start < 0 {
		return value
	}
	rest := value[quote_start + 1..]
	quote_end_offset := rest.index_u8(`"`)
	if quote_end_offset < 0 {
		return value
	}
	path := rest[..quote_end_offset]
	if path.len == 0 || os.is_abs_path(path) {
		return value
	}
	resolved := os.real_path(os.join_path_single(os.dir(source_file), path))
	quote_end := quote_start + 1 + quote_end_offset
	return value[..quote_start + 1] + resolved + value[quote_end..]
}

fn cached_resolve_relative_flag_paths(value string, source_file string) string {
	if source_file.len == 0 || !value.contains('/') {
		return value
	}
	base_dir := os.dir(source_file)
	if base_dir.len == 0 {
		return value
	}
	tokens, valid := cached_split_flag_tokens(value)
	if !valid {
		return value
	}
	mut resolved := []string{}
	for token in tokens {
		resolved << cached_resolve_relative_flag_path_token(token, base_dir)
	}
	return resolved.join(' ')
}

fn cached_split_flag_tokens(value string) ([]string, bool) {
	mut tokens := []string{}
	mut start := -1
	mut quote := u8(0)
	mut escaped := false
	for i, c in value.bytes() {
		if start < 0 {
			if c.is_space() {
				continue
			}
			start = i
		}
		if escaped {
			escaped = false
			continue
		}
		if c == `\\` {
			escaped = true
			continue
		}
		if quote != 0 {
			if c == quote {
				quote = 0
			}
			continue
		}
		if c in [`'`, `\"`] {
			quote = c
			continue
		}
		if c.is_space() {
			tokens << value[start..i]
			start = -1
		}
	}
	if quote != 0 {
		return []string{}, false
	}
	if start >= 0 {
		tokens << value[start..]
	}
	return tokens, true
}

fn cached_resolve_relative_flag_path_token(token string, base_dir string) string {
	for prefix in ['-I', '-L'] {
		if token.starts_with(prefix) && token.len > prefix.len {
			path := token[prefix.len..]
			return prefix + cached_resolve_relative_flag_path(path, base_dir)
		}
	}
	if !token.starts_with('-') {
		return cached_resolve_relative_flag_path(token, base_dir)
	}
	return token
}

fn cached_resolve_relative_flag_path(path_token string, base_dir string) string {
	mut path := path_token
	mut quote := u8(0)
	if path.len >= 2 && path[0] in [`'`, `\"`] && path[path.len - 1] == path[0] {
		quote = path[0]
		path = path[1..path.len - 1]
	}
	if !cached_flag_path_is_relative(path) {
		return path_token
	}
	resolved := os.real_path(os.join_path_single(base_dir, path))
	return if quote == 0 { resolved } else { quote.ascii_str() + resolved + quote.ascii_str() }
}

fn cached_flag_path_is_relative(path string) bool {
	if path.len == 0 || os.is_abs_path(path) {
		return false
	}
	return path.starts_with('./') || path.starts_with('../') || path.contains('/')
}

fn cached_vmod_root(source_file string) string {
	mut dir := os.dir(os.real_path(source_file))
	for dir.len > 0 {
		if os.is_file(os.join_path_single(dir, 'v.mod')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return os.dir(os.real_path(source_file))
}

fn import_text(a &flat.FlatAst, node flat.Node, import_paths map[string]string) string {
	import_path := import_paths[node.value] or { node.value }
	mut text := 'import ${import_path}'
	default_alias := import_path.all_after_last('.')
	if node.typ.len > 0 && node.typ != default_alias {
		text += ' as ${node.typ}'
	}
	if node.children_count > 0 {
		mut names := []string{}
		for i in 0 .. node.children_count {
			names << a.child_node(&node, i).value
		}
		text += ' { ${names.join(', ')} }'
	}
	return text
}

fn fn_text(a &flat.FlatAst, module_name string, node flat.Node, is_c bool, declaration_attrs []string) string {
	mut params := []flat.Node{}
	for i in 0 .. node.children_count {
		child := a.child_node(&node, i)
		if child.kind == .param {
			params << child
		}
	}
	mut name := node.value
	visibility := if !is_c && node.op == .arrow { 'pub ' } else { '' }
	mut head := if is_c { 'fn C.${name}' } else { '${visibility}fn ${name}' }
	mut param_start := 0
	if !is_c && name.contains('.') && params.len > 0 {
		receiver_type := name.all_before_last('.')
		first_type := clean_receiver_type(params[0].typ)
		if first_type == receiver_type
			|| first_type.all_after_last('.') == receiver_type.all_after_last('.') {
			receiver := params[0]
			receiver_name := if receiver.value.len > 0 { receiver.value } else { 'it' }
			mut receiver_decl_type := receiver.typ
			mut receiver_prefix := ''
			if receiver.is_mut {
				receiver_prefix = 'mut '
				receiver_decl_type = receiver_decl_type.trim_left('&').trim_space()
			} else if receiver_decl_type.starts_with('shared ') {
				receiver_prefix = 'shared '
				receiver_decl_type = receiver_decl_type['shared '.len..].trim_space()
			}
			head = '${visibility}fn (${receiver_prefix}${receiver_name} ${receiver_decl_type}) ${name.all_after_last('.')}'
			param_start = 1
		}
	}
	if node.generic_params.len > 0 {
		head += '[${node.generic_params.join(', ')}]'
	}
	mut ptexts := []string{}
	for i := param_start; i < params.len; i++ {
		p := params[i]
		mut pname := p.value
		if pname.len == 0 {
			pname = 'arg${i - param_start}'
		}
		mut ptype := p.typ
		mut prefix := ''
		if ptype.starts_with('&') && p.is_mut {
			prefix = 'mut '
			ptype = ptype[1..].trim_space()
		}
		ptexts << '${prefix}${pname} ${ptype}'
	}
	head += '(${ptexts.join(', ')})'
	if node.typ.len > 0 && node.typ != 'void' {
		head += ' ${node.typ}'
	}
	mut attr_lines := []string{}
	if fn_is_disabled(a, module_name, name) {
		attr_lines << '@[if false]'
	}
	mut attrs := []string{}
	if !cached_declaration_has_attr(declaration_attrs, 'export') {
		if export_name := fn_export_name(a, module_name, name) {
			attrs << "export: '${escape_v_string(export_name)}'"
		}
	}
	if !cached_declaration_has_attr(declaration_attrs, 'noreturn')
		&& fn_is_noreturn(a, module_name, name) {
		attrs << 'noreturn'
	}
	if attrs.len > 0 {
		attr_lines << '@[${attrs.join('; ')}]'
	}
	if attr_lines.len > 0 {
		return '${attr_lines.join('\n')}\n${head}'
	}
	return head
}

fn fn_is_disabled(a &flat.FlatAst, module_name string, name string) bool {
	if module_name.len == 0 || module_name in ['main', 'builtin'] {
		return name in a.disabled_fns
	}
	qualified_name := if name.starts_with('${module_name}.') {
		name
	} else {
		'${module_name}.${name}'
	}
	if qualified_name in a.disabled_fns {
		return true
	}
	short_module := module_name.all_after_last('.')
	short_qualified_name := if name.starts_with('${short_module}.') {
		name
	} else {
		'${short_module}.${name}'
	}
	return short_qualified_name in a.disabled_fns
}

fn fn_export_name(a &flat.FlatAst, module_name string, name string) ?string {
	if module_name.len == 0 || module_name in ['main', 'builtin'] {
		return a.export_fn_names[name] or { none }
	}
	qualified_name := if name.starts_with('${module_name}.') {
		name
	} else {
		'${module_name}.${name}'
	}
	if export_name := a.export_fn_names[qualified_name] {
		return export_name
	}
	short_module := module_name.all_after_last('.')
	short_qualified_name := if name.starts_with('${short_module}.') {
		name
	} else {
		'${short_module}.${name}'
	}
	return a.export_fn_names[short_qualified_name] or { none }
}

fn fn_is_noreturn(a &flat.FlatAst, module_name string, name string) bool {
	if module_name.len == 0 || module_name in ['main', 'builtin'] {
		return name in a.noreturn_fns
	}
	qualified_name := if name.starts_with('${module_name}.') {
		name
	} else {
		'${module_name}.${name}'
	}
	if qualified_name in a.noreturn_fns {
		return true
	}
	short_module := module_name.all_after_last('.')
	short_qualified_name := if name.starts_with('${short_module}.') {
		name
	} else {
		'${short_module}.${name}'
	}
	return short_qualified_name in a.noreturn_fns
}

fn clean_receiver_type(value string) string {
	mut typ := value.trim_space()
	for typ.starts_with('&') {
		typ = typ[1..].trim_space()
	}
	if typ.starts_with('shared ') {
		typ = typ['shared '.len..].trim_space()
	}
	return typ
}

fn generic_suffix(params []string) string {
	return if params.len > 0 { '[${params.join(', ')}]' } else { '' }
}

fn append_struct_field_nodes(a &flat.FlatAst, id flat.NodeId, mut fields []flat.NodeId) {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return
	}
	node := a.nodes[int(id)]
	if node.kind == .field_decl {
		fields << id
		return
	}
	if node.kind != .block {
		return
	}
	for i in 0 .. node.children_count {
		append_struct_field_nodes(a, a.child(&node, i), mut fields)
	}
}

fn struct_text(a &flat.FlatAst, node flat.Node, declaration_attrs []string) string {
	kind := if node.typ.split(',').contains('union') { 'union' } else { 'struct' }
	mut head := '${kind} ${node.value}${generic_suffix(node.generic_params)}'
	for part in node.typ.split(',') {
		if part.starts_with('implements=') {
			head += ' implements ' + part.all_after('=').replace('|', ', ')
		}
	}
	if node.typ.split(',').contains('params')
		&& !cached_declaration_has_attr(declaration_attrs, 'params') {
		head = '@[params]\n${head}'
	}
	if node.children_count == 0 {
		return head
	}
	mut out := strings.new_builder(256)
	out.writeln('${head} {')
	mut section := ''
	mut field_ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		append_struct_field_nodes(a, a.child(&node, i), mut field_ids)
	}
	for field_id in field_ids {
		field := a.node(field_id)
		flags := if field.generic_params.len > 0 { field.generic_params[0] } else { '' }
		wanted := if flags.contains('p') && flags.contains('m') {
			'pub mut'
		} else if flags.contains('p') {
			'pub'
		} else if flags.contains('m') {
			'mut'
		} else {
			''
		}
		if wanted != section && wanted.len > 0 {
			out.writeln('${wanted}:')
			section = wanted
		}
		for ai := 1; ai < field.generic_params.len; ai++ {
			out.writeln('\t@[${field.generic_params[ai]}]')
		}
		mut line := '\t${field.value}'
		if field.typ.len > 0 && field.typ != field.value {
			line += ' ${field.typ}'
		}
		if field.children_count > 0 {
			value := expr_text(a, a.child(field, 0))
			if value.len > 0 {
				line += ' = ${value}'
			}
		}
		out.writeln(line)
	}
	out.write_string('}')
	return out.str()
}

fn global_text(a &flat.FlatAst, tc &types.TypeChecker, module_name string, node flat.Node) string {
	mut out := strings.new_builder(128)
	out.writeln('__global (')
	for i in 0 .. node.children_count {
		field := a.child_node(&node, i)
		mut line := '\t${field.value}'
		mut field_type := field.typ
		if field_type.len == 0 {
			field_type = cached_global_type_name(tc, module_name, field.value)
		}
		if field_type.len > 0 {
			line += ' ${field_type}'
		}
		if field.children_count > 0 {
			value := expr_text(a, a.child(field, 0))
			if value.len > 0 {
				line += ' = ${value}'
			}
		}
		out.writeln(line)
	}
	out.write_string(')')
	return out.str()
}

fn cached_global_type_name(tc &types.TypeChecker, module_name string, name string) string {
	for candidate in [name, '${module_name}.${name}', '${module_name.all_after_last('.')}.${name}'] {
		if typ := tc.file_scope.lookup(candidate) {
			mut type_name := typ.name()
			if type_name == 'nil' || type_name == 'none' || type_name == 'unknown' {
				return 'voidptr'
			}
			if type_name.starts_with('&void[') {
				type_name = 'voidptr' + type_name['&void'.len..]
			}
			return type_name
		}
	}
	return ''
}

fn const_text(a &flat.FlatAst, node flat.Node) string {
	mut out := strings.new_builder(128)
	out.writeln('const (')
	for i in 0 .. node.children_count {
		field := a.child_node(&node, i)
		mut line := '\t${field.value}'
		if field.children_count > 0 {
			value := expr_text(a, a.child(field, 0))
			if value.len > 0 {
				line += ' = ${value}'
			} else {
				line += ' int'
			}
		} else if field.typ.len > 0 {
			line += ' ${field.typ}'
		} else {
			line += ' int'
		}
		out.writeln(line)
	}
	out.write_string(')')
	return out.str()
}

fn enum_text(a &flat.FlatAst, node flat.Node, declaration_attrs []string) string {
	mut out := strings.new_builder(128)
	if node.typ == 'flag' && !cached_declaration_has_attr(declaration_attrs, 'flag') {
		out.writeln('@[flag]')
	}
	if node.generic_params.contains('json_as_number')
		&& !cached_declaration_has_attr(declaration_attrs, 'json_as_number') {
		out.writeln('@[json_as_number]')
	}
	mut head := 'enum ${node.value}'
	if node.generic_params.len > 0 && node.generic_params[0].len > 0 {
		head += ' as ${node.generic_params[0]}'
	}
	out.writeln('${head} {')
	for i in 0 .. node.children_count {
		field := a.child_node(&node, i)
		for attr in field.generic_params {
			out.writeln('\t@[${attr}]')
		}
		mut line := '\t${field.value}'
		if field.children_count > 0 {
			value := expr_text(a, a.child(field, 0))
			if value.len > 0 {
				line += ' = ${value}'
			}
		}
		out.writeln(line)
	}
	out.write_string('}')
	return out.str()
}

fn type_text(a &flat.FlatAst, node flat.Node) string {
	head := 'type ${node.value}${generic_suffix(node.generic_params)} = '
	if node.children_count == 0 {
		return head + node.typ
	}
	mut variants := []string{}
	for i in 0 .. node.children_count {
		variants << a.child_node(&node, i).value
	}
	return head + variants.join(' | ')
}

fn interface_text(a &flat.FlatAst, node flat.Node) string {
	mut out := strings.new_builder(128)
	out.writeln('interface ${node.value}${generic_suffix(node.generic_params)} {')
	for i in 0 .. node.children_count {
		field := a.child_node(&node, i)
		if field.op == .dot {
			mut params := []string{}
			for pi in 0 .. field.children_count {
				param := a.child_node(field, pi)
				prefix := if param.op == .amp { 'mut ' } else { '' }
				param_type := if param.op == .amp { param.typ.trim_left('&') } else { param.typ }
				params << '${prefix}arg${pi} ${param_type}'
			}
			ret := if field.typ.len > 0 { ' ${field.typ}' } else { '' }
			out.writeln('\t${field.value}(${params.join(', ')})${ret}')
		} else if field.typ.len > 0 {
			out.writeln('\t${field.value} ${field.typ}')
		} else {
			out.writeln('\t${field.value}')
		}
	}
	out.write_string('}')
	return out.str()
}

fn expr_text(a &flat.FlatAst, id flat.NodeId) string {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return ''
	}
	node := a.nodes[int(id)]
	return match node.kind {
		.int_literal, .float_literal, .bool_literal, .ident {
			node.value
		}
		.char_literal {
			'`${escape_v_char(node.value)}`'
		}
		.string_literal {
			"'${escape_v_string(node.value)}'"
		}
		.nil_literal {
			'nil'
		}
		.none_expr {
			'none'
		}
		.paren {
			'(${expr_text(a, a.child(&node, 0))})'
		}
		.prefix {
			'${op_text(node.op)}${expr_text(a, a.child(&node, 0))}'
		}
		.postfix {
			'${expr_text(a, a.child(&node, 0))}${op_text(node.op)}'
		}
		.infix {
			'${expr_text(a, a.child(&node, 0))} ${op_text(node.op)} ${expr_text(a,
				a.child(&node, 1))}'
		}
		.selector {
			if node.children_count > 0 {
				'${expr_text(a, a.child(&node, 0))}.${node.value}'
			} else {
				node.value
			}
		}
		.enum_val {
			if node.value.starts_with('.') {
				node.value
			} else {
				'.${node.value}'
			}
		}
		.call {
			call_expr_text(a, node)
		}
		.cast_expr, .as_expr {
			target := if node.typ.len > 0 { node.typ } else { node.value }
			'${target}(${expr_text(a, a.child(&node, 0))})'
		}
		.sizeof_expr {
			'sizeof(${node.value})'
		}
		.typeof_expr {
			'typeof(${expr_text(a, a.child(&node, 0))})'
		}
		.array_literal {
			list_expr_text(a, node, '[', ']')
		}
		.array_init {
			array_init_expr_text(a, node)
		}
		.map_init {
			map_expr_text(a, node)
		}
		.struct_init {
			struct_init_expr_text(a, node)
		}
		.field_init {
			if node.children_count > 0 {
				'${node.value}: ${expr_text(a, a.child(&node, 0))}'
			} else {
				node.value
			}
		}
		.block {
			if node.children_count == 1 {
				expr_text(a, a.child(&node, 0))
			} else {
				''
			}
		}
		else {
			''
		}
	}
}

fn expr_can_serialize(a &flat.FlatAst, id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return false
	}
	node := a.nodes[int(id)]
	return match node.kind {
		.int_literal, .float_literal, .bool_literal, .ident, .char_literal, .string_literal,
		.nil_literal, .none_expr, .enum_val, .sizeof_expr {
			true
		}
		.paren, .prefix, .postfix, .typeof_expr {
			node.children_count == 1 && expr_can_serialize(a, a.child(&node, 0))
		}
		.infix {
			node.children_count == 2 && expr_children_can_serialize(a, node)
		}
		.selector {
			node.children_count == 0
				|| (node.children_count == 1 && expr_can_serialize(a, a.child(&node, 0)))
		}
		.call, .array_literal, .array_init, .struct_init {
			expr_children_can_serialize(a, node)
		}
		.map_init {
			(node.value.len > 0 || node.typ.len > 0) && node.children_count % 2 == 0
				&& expr_children_can_serialize(a, node)
		}
		.cast_expr, .as_expr {
			node.children_count == 1 && expr_can_serialize(a, a.child(&node, 0))
		}
		.field_init {
			node.children_count == 0
				|| (node.children_count == 1 && expr_can_serialize(a, a.child(&node, 0)))
		}
		.block {
			node.children_count == 1 && expr_can_serialize(a, a.child(&node, 0))
		}
		else {
			false
		}
	}
}

fn expr_children_can_serialize(a &flat.FlatAst, node flat.Node) bool {
	for i in 0 .. node.children_count {
		if !expr_can_serialize(a, a.child(&node, i)) {
			return false
		}
	}
	return true
}

fn array_init_expr_text(a &flat.FlatAst, node flat.Node) string {
	mut values := []string{}
	for i in 0 .. node.children_count {
		values << expr_text(a, a.child(&node, i))
	}
	typ := if node.typ.len > 0 { node.typ } else { node.value }
	return '${typ}{${values.join(', ')}}'
}

fn call_expr_text(a &flat.FlatAst, node flat.Node) string {
	if node.children_count == 0 {
		return '${node.value}()'
	}
	callee := expr_text(a, a.child(&node, 0))
	mut args := []string{}
	for i in 1 .. node.children_count {
		args << expr_text(a, a.child(&node, i))
	}
	return '${callee}(${args.join(', ')})'
}

fn list_expr_text(a &flat.FlatAst, node flat.Node, open string, close string) string {
	mut values := []string{}
	for i in 0 .. node.children_count {
		values << expr_text(a, a.child(&node, i))
	}
	prefix := if node.typ.len > 0 { node.typ } else { '' }
	return '${prefix}${open}${values.join(', ')}${close}'
}

fn map_expr_text(a &flat.FlatAst, node flat.Node) string {
	mut values := []string{}
	for i := 0; i + 1 < node.children_count; i += 2 {
		values << '${expr_text(a, a.child(&node, i))}: ${expr_text(a, a.child(&node, i + 1))}'
	}
	prefix := if node.value.len > 0 {
		node.value
	} else if node.typ.len > 0 {
		node.typ
	} else {
		'map[string]string'
	}
	return '${prefix}{${values.join(', ')}}'
}

fn struct_init_expr_text(a &flat.FlatAst, node flat.Node) string {
	mut values := []string{}
	for i in 0 .. node.children_count {
		values << expr_text(a, a.child(&node, i))
	}
	return '${node.value}{${values.join(', ')}}'
}

fn op_text(op flat.Op) string {
	return match op {
		.plus { '+' }
		.minus { '-' }
		.mul { '*' }
		.div { '/' }
		.mod { '%' }
		.eq { '==' }
		.ne { '!=' }
		.lt { '<' }
		.gt { '>' }
		.le { '<=' }
		.ge { '>=' }
		.amp { '&' }
		.pipe { '|' }
		.xor { '^' }
		.left_shift { '<<' }
		.right_shift { '>>' }
		.right_shift_unsigned { '>>>' }
		.logical_and { '&&' }
		.logical_or { '||' }
		.not { '!' }
		.bit_not { '~' }
		else { '' }
	}
}

fn escape_v_string(value string) string {
	mut out := strings.new_builder(value.len)
	for i, c in value.bytes() {
		match c {
			`\\` {
				out.write_u8(`\\`)
				out.write_u8(`\\`)
			}
			`'` {
				out.write_u8(`\\`)
				out.write_u8(`'`)
			}
			`\n` {
				out.write_string('\\n')
			}
			`\r` {
				out.write_string('\\r')
			}
			`\t` {
				out.write_string('\\t')
			}
			`$` {
				if i + 1 < value.len && (value[i + 1] == `{` || value[i + 1].is_letter()
					|| value[i + 1] == `_`) {
					out.write_u8(`\\`)
				}
				out.write_u8(c)
			}
			else {
				out.write_u8(c)
			}
		}
	}
	return out.str()
}

fn escape_v_char(value string) string {
	return value.replace('\\', '\\\\').replace('`', '\\`').replace('\n', '\\n').replace('\r', '\\r').replace('\t',
		'\\t')
}
