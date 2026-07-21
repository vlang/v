module modulecache

import os
import rand
import strings
import v3.flat
import v3.types

pub const builtin_bundle_imports = ['strconv', 'strings', 'hash', 'math.bits']
pub const builtin_bundle_modules = ['builtin', 'strconv', 'strings', 'hash', 'bits', 'math.bits']

const cache_format = 'v3-module-cache-45'
const c_body_begin = '/* V3CACHE_BODY_BEGIN */'
const c_body_end = '/* V3CACHE_BODY_END */'
const c_module_prefix = '/* V3CACHE_MODULE '
const c_native_directives_begin = '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */'
const c_native_directives_end = '/* V3CACHE_NATIVE_DIRECTIVES_END */'
const c_source_directives_begin = '/* V3CACHE_SOURCE_DIRECTIVES_BEGIN */'
const c_source_directives_end = '/* V3CACHE_SOURCE_DIRECTIVES_END */'
const c_late_directives_begin = '/* V3CACHE_LATE_DIRECTIVES_BEGIN */'
const c_late_directives_end = '/* V3CACHE_LATE_DIRECTIVES_END */'
const source_body_marker = '// v3cache: source bodies required'
const source_signature_cache_format = 'v3-source-signature-cache-1'

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
	source_bodies       bool
	source_bodies_known bool
pub:
	header       string
	object       string
	header_stamp string
	object_stamp string
	c_source     string
}

// CgenEntry contains the persistent whole-program C generation artifacts.
pub struct CgenEntry {
pub:
	source           string
	metadata         string
	stamp            string
	prepared_main    string
	prepared_tcc     string
	prepared_prefix  string
	prepared_objects string
	prepared_stamp   string
}

// CgenPreparedEntry contains cache-marked C sources already split for linking.
pub struct CgenPreparedEntry {
pub:
	main   string
	tcc    string
	prefix string
}

// GenericProgramEntry contains program-specific dependency specializations that
// remain valid across edits which do not change the program's generic ABI.
pub struct GenericProgramEntry {
pub:
	specs        string
	used         string
	prefix       string
	declarations string
	body         string
	literals     string
	metadata     string
	stamp        string
}

// IncrementalProgramEntry contains a complete program body split into stable
// function sections plus the semantic/link metadata needed to regenerate only
// functions whose parsed bodies changed.
pub struct IncrementalProgramEntry {
pub:
	manifest         string
	body             string
	used             string
	specs            string
	prefix           string
	declarations     string
	tcc_declarations string
	objects          string
	metadata         string
	stamp            string
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

// cgen_entry returns the artifact paths for one stable program source set.
pub fn (m &Manager) cgen_entry(source_files []string) CgenEntry {
	mut paths := source_files.map(os.real_path(it))
	paths.sort()
	id := hash_text(paths.join('\n'))
	base := os.join_path(m.dir, 'program_${id}')
	return CgenEntry{
		source:           '${base}.c'
		metadata:         '${base}.cflags'
		stamp:            '${base}.c.stamp'
		prepared_main:    '${base}.main.c'
		prepared_tcc:     '${base}.tcc.c'
		prepared_prefix:  '${base}.prefix.c'
		prepared_objects: '${base}.objects'
		prepared_stamp:   '${base}.prepared.stamp'
	}
}

fn (m &Manager) generic_program_entry(source_files []string) GenericProgramEntry {
	cgen := m.cgen_entry(source_files)
	base := cgen.source.all_before_last('.c')
	return GenericProgramEntry{
		specs:        '${base}.generic.specs'
		used:         '${base}.generic.used'
		prefix:       '${base}.generic.prefix.c'
		declarations: '${base}.generic.declarations.c'
		body:         '${base}.generic.body.c'
		literals:     '${base}.generic.literals'
		metadata:     '${base}.generic.metadata'
		stamp:        '${base}.generic.stamp'
	}
}

fn (m &Manager) incremental_program_entry(source_files []string) IncrementalProgramEntry {
	cgen := m.cgen_entry(source_files)
	base := cgen.source.all_before_last('.c')
	return IncrementalProgramEntry{
		manifest:         '${base}.incremental.manifest'
		body:             '${base}.incremental.body.c'
		used:             '${base}.incremental.used'
		specs:            '${base}.incremental.specs'
		prefix:           '${base}.incremental.prefix.c'
		declarations:     '${base}.incremental.declarations.c'
		tcc_declarations: '${base}.incremental.tcc.declarations.c'
		objects:          '${base}.incremental.objects'
		metadata:         '${base}.incremental.metadata'
		stamp:            '${base}.incremental.stamp'
	}
}

// source_signature hashes selected source paths, contents, resolved module roots,
// build/environment values, and pkg-config probe results in stable order.
pub fn source_signature(source_files []string) string {
	return source_signature_details(source_files, '').signature
}

struct SourceSignatureDetails {
	signature  string
	validation []string
}

fn source_signature_details(source_files []string, build_pseudo_values string) SourceSignatureDetails {
	mut files := source_files.clone()
	files.sort()
	mut hash := u64(1469598103934665603)
	mut env_names := map[string]bool{}
	mut pkgconfig_names := map[string]bool{}
	mut uses_build_pseudo := false
	mut validation := []string{}
	for file in files {
		path := os.real_path(file)
		hash = hash_bytes(hash, path.bytes())
		hash = hash_bytes(hash, [u8(0)])
		content := os.read_bytes(file) or { return SourceSignatureDetails{} }
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
			vmod_metadata := if vmod_file.len > 0 {
				file_metadata_signature(vmod_file)
			} else {
				''
			}
			validation << 'vmod=${path}\t${root}\t${vmod_file}\t${vmod_metadata}'
			hash = hash_bytes(hash, [u8(0xfc)])
			hash = hash_bytes(hash, root.bytes())
			hash = hash_bytes(hash, [u8(0)])
			if vmod_file.len > 0 {
				vmod_content := os.read_bytes(vmod_file) or { return SourceSignatureDetails{} }
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
		validation << 'build=${hash_text(build_pseudo_values)}'
		hash = hash_bytes(hash, [u8(0xfb)])
		hash = hash_bytes(hash, build_pseudo_values.bytes())
		hash = hash_bytes(hash, [u8(0xff)])
	}
	mut names := env_names.keys()
	names.sort()
	for name in names {
		value := os.getenv(name)
		validation << 'env=${name}\t${hash_text(value)}'
		hash = hash_bytes(hash, [u8(0xfe)])
		hash = hash_bytes(hash, name.bytes())
		hash = hash_bytes(hash, [u8(0)])
		hash = hash_bytes(hash, value.bytes())
		hash = hash_bytes(hash, [u8(0xff)])
	}
	mut packages := pkgconfig_names.keys()
	packages.sort()
	for name in packages {
		available := os.execute('pkg-config --exists ${name}').exit_code == 0
		validation << 'pkg=${name}\t${if available { 1 } else { 0 }}'
		hash = hash_bytes(hash, [u8(0xfd)])
		hash = hash_bytes(hash, name.bytes())
		hash = hash_bytes(hash, [u8(0)])
		hash = hash_bytes(hash, [u8(if available { 1 } else { 0 })])
		hash = hash_bytes(hash, [u8(0xff)])
	}
	return SourceSignatureDetails{
		signature:  hash.hex()
		validation: validation
	}
}

fn (m &Manager) source_signature(source_files []string) string {
	return cached_source_signature_with_build_values(m.dir, 'module', source_files,
		m.build_pseudo_values)
}

// cached_source_signature returns a content signature while using precise file
// metadata to avoid rereading unchanged inputs on subsequent compiler runs.
pub fn cached_source_signature(cache_dir string, namespace string, source_files []string) string {
	return cached_source_signature_with_build_values(cache_dir, namespace, source_files, '')
}

fn cached_source_signature_with_build_values(cache_dir string, namespace string, source_files []string, build_pseudo_values string) string {
	mut paths := source_files.map(os.real_path(it))
	paths.sort()
	cache_key := hash_text(namespace + '\n' + paths.join('\n'))
	cache_path := os.join_path(cache_dir, '.source_signature_${cache_key}')
	metadata := source_files_metadata_signature(paths)
	if metadata.len > 0 {
		cached := os.read_file(cache_path) or { '' }
		if signature := valid_cached_source_signature(cached, metadata, build_pseudo_values) {
			return signature
		}
	}
	details := source_signature_details(paths, build_pseudo_values)
	if details.signature.len == 0 {
		return ''
	}
	fresh_metadata := source_files_metadata_signature(paths)
	if fresh_metadata.len > 0 {
		mut out := strings.new_builder(192 + details.validation.len * 96)
		out.writeln('format=${source_signature_cache_format}')
		out.writeln('metadata=${fresh_metadata}')
		for input in details.validation {
			out.writeln(input)
		}
		out.writeln('source=${details.signature}')
		out.writeln('complete=1')
		os.mkdir_all(cache_dir) or {}
		if os.is_dir(cache_dir) {
			write_atomic(cache_path, out.str()) or {}
		}
	}
	return details.signature
}

fn source_files_metadata_signature(paths []string) string {
	mut hash := u64(1469598103934665603)
	for path in paths {
		metadata := file_metadata_signature(path)
		if metadata.len == 0 {
			return ''
		}
		hash = hash_bytes(hash, path.bytes())
		hash = hash_bytes(hash, [u8(0)])
		hash = hash_bytes(hash, metadata.bytes())
		hash = hash_bytes(hash, [u8(0xff)])
	}
	return hash.hex()
}

fn valid_cached_source_signature(content string, metadata string, build_pseudo_values string) ?string {
	lines := content.split_into_lines()
	if lines.len < 4 || lines[0] != 'format=${source_signature_cache_format}'
		|| lines[1] != 'metadata=${metadata}' || lines.last() != 'complete=1' {
		return none
	}
	mut signature := ''
	for line in lines[2..lines.len - 1] {
		if line.starts_with('source=') {
			if signature.len > 0 {
				return none
			}
			signature = line.all_after('source=')
			continue
		}
		if line.starts_with('build=') {
			if line != 'build=${hash_text(build_pseudo_values)}' {
				return none
			}
			continue
		}
		if line.starts_with('env=') {
			parts := line['env='.len..].split('\t')
			if parts.len != 2 || parts[0].len == 0 || hash_text(os.getenv(parts[0])) != parts[1] {
				return none
			}
			continue
		}
		if line.starts_with('pkg=') {
			parts := line['pkg='.len..].split('\t')
			if parts.len != 2 || parts[0].len == 0 {
				return none
			}
			available := os.execute('pkg-config --exists ${parts[0]}').exit_code == 0
			if parts[1] != '${if available {
				1
			} else {
				0
			}}' {
				return none
			}
			continue
		}
		if line.starts_with('vmod=') {
			parts := line['vmod='.len..].split('\t')
			if parts.len != 4 || parts[0].len == 0 {
				return none
			}
			root, vmod_file := signature_vmod_root(parts[0])
			vmod_metadata := if vmod_file.len > 0 {
				file_metadata_signature(vmod_file)
			} else {
				''
			}
			if root != parts[1] || vmod_file != parts[2] || vmod_metadata != parts[3] {
				return none
			}
			continue
		}
		return none
	}
	if signature.len == 0 {
		return none
	}
	return signature
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
	if !source.contains('\$env') {
		return []
	}
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
	if !source.contains('pkgconfig') {
		return []
	}
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
	mut dependency_metadata := map[string]string{}
	return m.valid_entry_with_metadata_cache(module_name, source_files, mut dependency_metadata)
}

// valid_entry_with_metadata_cache reports whether both interface and object
// artifacts match their sources, reusing dependency metadata already observed
// during this compiler run.
pub fn (m &Manager) valid_entry_with_metadata_cache(module_name string, source_files []string, mut dependency_metadata map[string]string) ?Entry {
	if !m.enabled || source_files.len == 0 {
		return none
	}
	entry := m.entry(module_name, source_files)
	if !os.is_file(entry.header) {
		return none
	}
	stamp := os.read_file(entry.header_stamp) or { return none }
	expected := entry_stamp(m.salt, m.source_signature(source_files))
	source_bodies := header_stamp_source_bodies(stamp, expected) or { return none }
	object_stamp := os.read_file(entry.object_stamp) or { return none }
	if !object_stamp_valid_with_metadata_cache(object_stamp, expected, mut dependency_metadata) {
		return none
	}
	return Entry{
		...entry
		source_bodies:       source_bodies
		source_bodies_known: true
	}
}

// valid_header reports whether a declaration header matches its module sources.
pub fn (m &Manager) valid_header(module_name string, source_files []string) ?Entry {
	if !m.enabled || source_files.len == 0 {
		return none
	}
	entry := m.entry(module_name, source_files)
	if !os.is_file(entry.header) {
		return none
	}
	stamp := os.read_file(entry.header_stamp) or { return none }
	expected := entry_stamp(m.salt, m.source_signature(source_files))
	source_bodies := header_stamp_source_bodies(stamp, expected) or { return none }
	return Entry{
		...entry
		source_bodies:       source_bodies
		source_bodies_known: true
	}
}

// header_needs_source reports whether a declaration header has bodies that must
// remain available to per-program monomorphization.
pub fn header_needs_source(entry Entry) bool {
	if !entry.source_bodies_known {
		header := os.read_file(entry.header) or { return true }
		return header.contains(source_body_marker)
	}
	return entry.source_bodies
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
	write_atomic(entry.header_stamp, header_entry_stamp(m.salt, m.source_signature(source_files),
		header))!
	return entry
}

// write_header commits one declaration-only module header and its source stamp.
pub fn (m &Manager) write_header(module_name string, source_files []string, header string) !Entry {
	if !m.ensure_dir() {
		return error('v3 module cache directory is unavailable')
	}
	entry := m.entry(module_name, source_files)
	write_atomic(entry.header, header)!
	write_atomic(entry.header_stamp, header_entry_stamp(m.salt, m.source_signature(source_files),
		header))!
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

// valid_cgen reports whether a whole-program C plan matches its program sources,
// imported module headers, and semantic generation signature.
pub fn (m &Manager) valid_cgen(source_files []string, generation_signature string, dependency_inputs map[string]string) ?CgenEntry {
	if !m.enabled || source_files.len == 0 {
		return none
	}
	entry := m.cgen_entry(source_files)
	if !os.is_file(entry.source) || !os.is_file(entry.metadata) || !os.is_file(entry.stamp) {
		return none
	}
	stamp := os.read_file(entry.stamp) or { return none }
	expected := cgen_entry_stamp(m.salt, m.source_signature(source_files), dependency_inputs,
		generation_signature)
	if stamp != expected {
		return none
	}
	return entry
}

// valid_generic_program reports whether cached dependency specializations match
// the program's type/call shape and all module cache inputs.
pub fn (m &Manager) valid_generic_program(source_files []string, semantic_signature string, generation_signature string, dependency_inputs map[string]string) ?GenericProgramEntry {
	if !m.enabled || source_files.len == 0 || semantic_signature.len == 0 {
		return none
	}
	entry := m.generic_program_entry(source_files)
	if !os.is_file(entry.specs) || !os.is_file(entry.used) || !os.is_file(entry.prefix)
		|| !os.is_file(entry.declarations) || !os.is_file(entry.body) || !os.is_file(entry.literals)
		|| !os.is_file(entry.metadata) || !os.is_file(entry.stamp) {
		return none
	}
	stamp := os.read_file(entry.stamp) or { return none }
	expected := cgen_entry_stamp(m.salt, semantic_signature, dependency_inputs,
		'generic-v6\n${generation_signature}')
	if stamp != expected {
		return none
	}
	return entry
}

// write_cgen atomically commits a whole-program C plan and its generation metadata.
pub fn (m &Manager) write_cgen(source_files []string, generation_signature string, dependency_inputs map[string]string, source string, metadata string) !CgenEntry {
	if !m.ensure_dir() {
		return error('v3 module cache directory is unavailable')
	}
	entry := m.cgen_entry(source_files)
	// The stamp is the commit marker. Remove the previous one before replacing
	// either payload so concurrent readers never accept a mixed generation.
	os.rm(entry.stamp) or {}
	os.rm(entry.prepared_stamp) or {}
	write_atomic(entry.source, source)!
	write_atomic(entry.metadata, metadata)!
	stamp := cgen_entry_stamp(m.salt, m.source_signature(source_files), dependency_inputs,
		generation_signature)
	write_atomic(entry.stamp, stamp)!
	return entry
}

// write_generic_program atomically publishes dependency specialization
// metadata. The stamp is written last so readers cannot observe mixed payloads.
pub fn (m &Manager) write_generic_program(source_files []string, semantic_signature string, generation_signature string, dependency_inputs map[string]string, specs string, used string, prefix string, declarations string, body string, literals string, metadata string) !GenericProgramEntry {
	if !m.ensure_dir() {
		return error('v3 module cache directory is unavailable')
	}
	entry := m.generic_program_entry(source_files)
	os.rm(entry.stamp) or {}
	write_atomic(entry.specs, specs)!
	write_atomic(entry.used, used)!
	write_atomic(entry.prefix, prefix)!
	write_atomic(entry.declarations, declarations)!
	write_atomic(entry.body, body)!
	write_atomic(entry.literals, literals)!
	write_atomic(entry.metadata, metadata)!
	stamp := cgen_entry_stamp(m.salt, semantic_signature, dependency_inputs,
		'generic-v6\n${generation_signature}')
	write_atomic(entry.stamp, stamp)!
	return entry
}

// valid_incremental_program restores function-level artifacts when declarations,
// compiler configuration, dependencies, and native inputs are unchanged.
pub fn (m &Manager) valid_incremental_program(source_files []string, declaration_signature string, generation_signature string, dependency_inputs map[string]string) ?IncrementalProgramEntry {
	if !m.enabled || source_files.len == 0 || declaration_signature.len == 0 {
		return none
	}
	entry := m.incremental_program_entry(source_files)
	if !os.is_file(entry.manifest) || !os.is_file(entry.body) || !os.is_file(entry.used)
		|| !os.is_file(entry.specs) || !os.is_file(entry.prefix) || !os.is_file(entry.declarations)
		|| !os.is_file(entry.tcc_declarations) || !os.is_file(entry.objects)
		|| !os.is_file(entry.metadata) || !os.is_file(entry.stamp) {
		return none
	}
	objects := os.read_lines(entry.objects) or { return none }
	if objects.len == 0 || objects.any(it.len == 0 || !os.is_file(it)) {
		return none
	}
	stamp := os.read_file(entry.stamp) or { return none }
	expected := cgen_entry_stamp(m.salt, declaration_signature, dependency_inputs,
		'incremental-v5\n${generation_signature}')
	if stamp != expected {
		return none
	}
	return entry
}

// write_incremental_program atomically publishes a function-level program
// snapshot. The stamp is the commit marker and is written after every payload.
pub fn (m &Manager) write_incremental_program(source_files []string, declaration_signature string, generation_signature string, dependency_inputs map[string]string, manifest string, body string, used string, specs string, prefix string, declarations string, tcc_declarations string, objects []string, metadata string) !IncrementalProgramEntry {
	if !m.ensure_dir() {
		return error('v3 module cache directory is unavailable')
	}
	entry := m.incremental_program_entry(source_files)
	os.rm(entry.stamp) or {}
	write_atomic(entry.manifest, manifest)!
	write_atomic(entry.body, body)!
	write_atomic(entry.used, used)!
	write_atomic(entry.specs, specs)!
	write_atomic(entry.prefix, prefix)!
	write_atomic(entry.declarations, declarations)!
	write_atomic(entry.tcc_declarations, tcc_declarations)!
	write_atomic(entry.objects, objects.join('\n'))!
	write_atomic(entry.metadata, metadata)!
	stamp := cgen_entry_stamp(m.salt, declaration_signature, dependency_inputs,
		'incremental-v5\n${generation_signature}')
	write_atomic(entry.stamp, stamp)!
	return entry
}

// valid_cgen_prepared reports whether all pre-split C plan sources match entry's generation.
pub fn (m &Manager) valid_cgen_prepared(entry CgenEntry) ?CgenPreparedEntry {
	if !os.is_file(entry.prepared_main) || !os.is_file(entry.prepared_tcc)
		|| !os.is_file(entry.prepared_prefix) || !os.is_file(entry.prepared_stamp) {
		return none
	}
	stamp := os.read_file(entry.stamp) or { return none }
	prepared_stamp := os.read_file(entry.prepared_stamp) or { return none }
	if prepared_stamp != stamp {
		return none
	}
	return CgenPreparedEntry{
		main:   entry.prepared_main
		tcc:    entry.prepared_tcc
		prefix: entry.prepared_prefix
	}
}

// write_cgen_prepared publishes pre-split C plan sources, committing their stamp last.
pub fn (m &Manager) write_cgen_prepared(entry CgenEntry, main_source string, tcc_source string, prefix_source string) ! {
	os.rm(entry.prepared_stamp) or {}
	write_atomic(entry.prepared_main, main_source)!
	write_atomic(entry.prepared_tcc, tcc_source)!
	write_atomic(entry.prepared_prefix, prefix_source)!
	stamp := os.read_file(entry.stamp)!
	write_atomic(entry.prepared_stamp, stamp)!
}

// valid_cgen_prepared_objects restores the object set for one effective compile signature.
pub fn (m &Manager) valid_cgen_prepared_objects(entry CgenEntry, compile_signature string) ?[]string {
	path := '${entry.prepared_objects}.${hash_text(compile_signature)}'
	content := os.read_file(path) or { return none }
	lines := content.split_into_lines()
	if lines.len < 2 {
		return none
	}
	stamp := os.read_file(entry.stamp) or { return none }
	if lines[0] != 'stamp=${hash_text(stamp)}' {
		return none
	}
	objects := lines[1..].filter(it.len > 0)
	if objects.len == 0 || objects.any(!os.is_file(it)) {
		return none
	}
	return objects
}

// write_cgen_prepared_objects publishes the object set for one effective compile signature.
pub fn (m &Manager) write_cgen_prepared_objects(entry CgenEntry, compile_signature string, objects []string) ! {
	stamp := os.read_file(entry.stamp)!
	content := 'stamp=${hash_text(stamp)}\n' + objects.join('\n') + '\n'
	path := '${entry.prepared_objects}.${hash_text(compile_signature)}'
	write_atomic(path, content)!
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
	// The temporary name must be unique per writer, not just per process: a
	// persistent worker pool can publish the same cache path from several
	// threads at once, and `${path}.tmp.${pid}` would collide between them.
	tmp := '${path}.tmp.${os.getpid()}.${rand.ulid()}'
	defer {
		os.rm(tmp) or {}
	}
	os.write_file(tmp, content)!
	os.mv(tmp, path)!
}

fn entry_stamp(salt string, source_hash string) string {
	return 'format=${cache_format}\nconfig=${hash_text(salt)}\nsource=${source_hash}\n'
}

fn header_entry_stamp(salt string, source_hash string, header string) string {
	return entry_stamp(salt, source_hash) +
		'source_bodies=${int(header.contains(source_body_marker))}\n'
}

fn header_stamp_source_bodies(stamp string, expected_entry string) ?bool {
	if stamp == expected_entry + 'source_bodies=0\n' {
		return false
	}
	if stamp == expected_entry + 'source_bodies=1\n' {
		return true
	}
	return none
}

fn object_entry_stamp(salt string, source_hash string, dependency_inputs map[string]string, compile_signature string) string {
	mut out := strings.new_builder(256 + dependency_inputs.len * 96)
	out.write_string(entry_stamp(salt, source_hash))
	out.writeln('compile=${hash_text(compile_signature)}')
	mut paths := dependency_inputs.keys()
	paths.sort()
	for path in paths {
		out.writeln('dependency=${path}\t${dependency_inputs[path]}\t${file_metadata_signature(path)}')
	}
	return out.str()
}

fn cgen_entry_stamp(salt string, source_hash string, dependency_inputs map[string]string, generation_signature string) string {
	mut out := strings.new_builder(256 + dependency_inputs.len * 96)
	out.write_string(entry_stamp(salt, source_hash))
	out.writeln('generation=${hash_text(generation_signature)}')
	mut paths := dependency_inputs.keys()
	paths.sort()
	for path in paths {
		out.writeln('dependency=${path}\t${dependency_inputs[path]}')
	}
	return out.str()
}

fn object_stamp_valid(stamp string, expected_entry string) bool {
	mut dependency_metadata := map[string]string{}
	return object_stamp_valid_with_metadata_cache(stamp, expected_entry, mut dependency_metadata)
}

fn object_stamp_valid_with_metadata_cache(stamp string, expected_entry string, mut dependency_metadata map[string]string) bool {
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
		dependency := parse_object_stamp_dependency(line) or { return false }
		current_metadata := dependency_metadata[dependency.path] or {
			metadata := file_metadata_signature(dependency.path)
			dependency_metadata[dependency.path] = metadata
			metadata
		}
		if dependency.metadata.len > 0 && current_metadata == dependency.metadata {
			continue
		}
		// Headers produced later in a cold cache build can have no metadata in an
		// earlier module's stamp. Hash each such dependency only once per compiler
		// run even when it appears in many transitive object stamps.
		signature_key := '\x00${dependency.path}'
		current_signature := dependency_metadata[signature_key] or {
			signature := file_signature(dependency.path)
			dependency_metadata[signature_key] = signature
			signature
		}
		if current_signature != dependency.signature {
			return false
		}
	}
	return has_compile_signature
}

struct ObjectStampDependency {
	path      string
	signature string
	metadata  string
}

fn parse_object_stamp_dependency(line string) ?ObjectStampDependency {
	if !line.starts_with('dependency=') {
		return none
	}
	value := line['dependency='.len..]
	metadata_tab := value.last_index_u8(`\t`)
	if metadata_tab <= 0 {
		return none
	}
	path_and_signature := value[..metadata_tab]
	signature_tab := path_and_signature.last_index_u8(`\t`)
	if signature_tab <= 0 || signature_tab + 1 >= path_and_signature.len {
		return none
	}
	return ObjectStampDependency{
		path:      path_and_signature[..signature_tab]
		signature: path_and_signature[signature_tab + 1..]
		metadata:  value[metadata_tab + 1..]
	}
}

fn object_stamp_dependencies_match(stamp string, expected map[string]string) bool {
	mut actual := map[string]string{}
	for line in stamp.split_into_lines() {
		if !line.starts_with('dependency=') {
			continue
		}
		dependency := parse_object_stamp_dependency(line) or { return false }
		if dependency.path in actual {
			return false
		}
		actual[dependency.path] = dependency.signature
	}
	for path, signature in expected {
		if path !in actual {
			return false
		}
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
	sections := [
		CDeclarationSection{
			begin: c_native_directives_begin
			end:   c_native_directives_end
			keep:  true
		},
		CDeclarationSection{
			begin: c_source_directives_begin
			end:   c_source_directives_end
		},
		CDeclarationSection{
			begin: c_late_directives_begin
			end:   c_late_directives_end
		},
	]
	mut out := strings.new_builder(prefix.len / 2)
	mut pos := 0
	for pos < prefix.len {
		mut next_pos := prefix.len
		mut selected := -1
		for i, section in sections {
			if relative := prefix[pos..].index(section.begin) {
				absolute := pos + relative
				if absolute < next_pos {
					next_pos = absolute
					selected = i
				}
			}
		}
		if selected < 0 {
			header, _ := c_declaration_header(prefix[pos..])
			out.write_string(header)
			break
		}
		if next_pos > pos {
			header, _ := c_declaration_header(prefix[pos..next_pos])
			out.write_string(header)
		}
		section := sections[selected]
		body_start := next_pos + section.begin.len
		relative_end := prefix[body_start..].index(section.end) or {
			header, _ := c_declaration_header(prefix[next_pos..])
			out.write_string(header)
			break
		}
		body_end := body_start + relative_end
		if section.keep {
			out.write_string(c_native_declaration_directives(prefix[body_start..body_end]))
		}
		pos = body_end + section.end.len
	}
	return out.str()
}

fn cached_c_string_symbol(value string) string {
	mut hash := u64(1469598103934665603)
	for c in value.bytes() {
		hash = (hash ^ u64(c)) * u64(1099511628211)
	}
	return '_v3_lit_${value.len}_${hash.hex()}'
}

fn cached_c_escape(value string) string {
	mut out := strings.new_builder(value.len * 2)
	for b in value.bytes() {
		match b {
			`\\` {
				out.write_string('\\\\')
			}
			`"` {
				out.write_string('\\"')
			}
			`\n` {
				out.write_string('\\n')
			}
			`\t` {
				out.write_string('\\t')
			}
			`\r` {
				out.write_string('\\r')
			}
			else {
				if b < 32 || b == 127 {
					out.write_u8(`\\`)
					out.write_u8(u8(`0` + ((b >> 6) & 7)))
					out.write_u8(u8(`0` + ((b >> 3) & 7)))
					out.write_u8(u8(`0` + (b & 7)))
				} else {
					out.write_u8(b)
				}
			}
		}
	}
	return out.str()
}

// rewrite_cached_runtime_strings updates cached C literal symbols as one simultaneous rewrite.
pub fn rewrite_cached_runtime_strings(cached_source string, old_values []string, new_values []string) ?string {
	if old_values.len != new_values.len {
		return none
	}
	mut unchanged_values := map[string]bool{}
	for idx, old_value in old_values {
		if old_value == new_values[idx] {
			unchanged_values[old_value] = true
		}
	}
	mut replacements := map[string]string{}
	for idx, old_value in old_values {
		new_value := new_values[idx]
		if old_value == new_value {
			continue
		}
		// Equal literals share one C symbol. Rewriting that symbol would also
		// change occurrences whose literal stayed the same.
		if unchanged_values[old_value] {
			return none
		}
		if existing := replacements[old_value] {
			if existing != new_value {
				return none
			}
		} else {
			replacements[old_value] = new_value
		}
	}
	if replacements.len == 0 {
		return cached_source.clone()
	}
	mut source := cached_source
	mut replacement_values := replacements.keys()
	replacement_values.sort()
	mut symbol_replacements := map[string]string{}
	mut target_definitions := map[string]string{}
	mut target_definitions_present := map[string]bool{}
	mut removed_definition_symbols := map[string]bool{}
	for old_value in replacement_values {
		new_value := replacements[old_value]
		old_symbol := cached_c_string_symbol(old_value)
		if !cached_source.contains(old_symbol) {
			continue
		}
		new_symbol := cached_c_string_symbol(new_value)
		old_definition := 'static string ${old_symbol} = {"${cached_c_escape(old_value)}", ${old_value.len}, 1};'
		new_definition := 'static string ${new_symbol} = {"${cached_c_escape(new_value)}", ${new_value.len}, 1};'
		if cached_source.contains(new_definition) {
			target_definitions_present[new_symbol] = true
		}
		if source.contains(old_definition) {
			source = source.replace('${old_definition}\n', '').replace(old_definition, '')
			removed_definition_symbols[old_symbol] = true
		}
		symbol_replacements[old_symbol] = new_symbol
		target_definitions[new_symbol] = new_definition
	}
	source = rewrite_c_identifier_tokens(source, symbol_replacements)
	mut definitions := []string{}
	mut target_symbols := target_definitions.keys()
	target_symbols.sort()
	for target_symbol in target_symbols {
		if !target_definitions_present[target_symbol] || removed_definition_symbols[target_symbol] {
			definitions << target_definitions[target_symbol]
		}
	}
	if definitions.len == 0 {
		return source
	}
	marker_idx := source.index(c_body_begin) or { return none }
	return source[..marker_idx] + definitions.join('\n') + '\n' + source[marker_idx..]
}

fn rewrite_c_identifier_tokens(source string, replacements map[string]string) string {
	if replacements.len == 0 {
		return source.clone()
	}
	mut out := strings.new_builder(source.len)
	mut last := 0
	mut i := 0
	mut quote := u8(0)
	mut escaped := false
	mut line_comment := false
	mut block_comment := false
	for i < source.len {
		c := source[i]
		next := if i + 1 < source.len { source[i + 1] } else { u8(0) }
		if line_comment {
			if c == `\n` {
				line_comment = false
			}
			i++
			continue
		}
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
			line_comment = true
			i += 2
			continue
		}
		if c == `/` && next == `*` {
			block_comment = true
			i += 2
			continue
		}
		if c in [`'`, `"`] {
			quote = c
			i++
			continue
		}
		if c_generated_identifier_byte(c) {
			start := i
			i++
			for i < source.len && c_generated_identifier_byte(source[i]) {
				i++
			}
			if replacement := replacements[source[start..i]] {
				out.write_string(source[last..start])
				out.write_string(replacement)
				last = i
			}
			continue
		}
		i++
	}
	if last == 0 {
		return source.clone()
	}
	out.write_string(source[last..])
	return out.str()
}

// prune_unreferenced_static_string_definitions removes generated string storage
// that is referenced only by the program body. Cached module objects carry
// their own static copies, while the program translation unit retains the full
// prefix. Keeping body-only strings out of the shared dylib prefix makes an
// ordinary program literal edit reuse the same cached dylib.
pub fn prune_unreferenced_static_string_definitions(prefix string) string {
	mut references := map[string]int{}
	for line in prefix.split_into_lines() {
		for symbol in generated_string_symbols(line) {
			references[symbol]++
		}
	}
	mut out := strings.new_builder(prefix.len)
	for line in prefix.split_into_lines() {
		if symbol := generated_static_string_definition_symbol(line) {
			if references[symbol] == 1 {
				continue
			}
		}
		out.writeln(line)
	}
	return out.str()
}

// static_string_definitions returns generated literal storage needed by the
// current program body. Cached dependency declarations omit these records.
pub fn static_string_definitions(source string) string {
	mut out := strings.new_builder(4096)
	for line in source.split_into_lines() {
		if line.starts_with('static string _v3_lit_') {
			out.writeln(line)
		}
	}
	return out.str()
}

// materialize_cached_body_string_definitions restores body-only string storage
// recorded as cache marker comments. Real definitions in source take precedence.
pub fn materialize_cached_body_string_definitions(source string) string {
	marker := '// V3CACHE_BASELINE '
	if !source.contains(marker) {
		return source.clone()
	}
	mut actual_symbols := map[string]bool{}
	for line in source.split_into_lines() {
		if line.starts_with(marker) {
			continue
		}
		if symbol := generated_static_string_definition_symbol(line) {
			actual_symbols[symbol] = true
		}
	}
	mut emitted_symbols := map[string]bool{}
	mut out := strings.new_builder(source.len)
	for line in source.split_into_lines() {
		if line.starts_with(marker) {
			definition := line[marker.len..]
			if symbol := generated_static_string_definition_symbol(definition) {
				if !actual_symbols[symbol] && !emitted_symbols[symbol] {
					out.writeln(definition)
					emitted_symbols[symbol] = true
				}
				continue
			}
		}
		if symbol := generated_static_string_definition_symbol(line) {
			if emitted_symbols[symbol] {
				continue
			}
			emitted_symbols[symbol] = true
		}
		out.writeln(line)
	}
	return out.str()
}

fn generated_static_string_definition_symbol(line string) ?string {
	clean := line.trim_space()
	prefix := 'static string '
	if !clean.starts_with(prefix) {
		return none
	}
	start := prefix.len
	mut end := start
	for end < clean.len && c_generated_identifier_byte(clean[end]) {
		end++
	}
	if end == start || !clean[start..end].starts_with('_v3_lit_') {
		return none
	}
	tail := clean[end..].trim_space()
	if !tail.starts_with('=') {
		return none
	}
	return clean[start..end]
}

fn generated_string_symbols(line string) []string {
	mut symbols := []string{}
	mut i := 0
	for i < line.len {
		relative := line[i..].index('_v3_lit_') or { break }
		start := i + relative
		if start > 0 && c_generated_identifier_byte(line[start - 1]) {
			i = start + 1
			continue
		}
		mut end := start + '_v3_lit_'.len
		for end < line.len && c_generated_identifier_byte(line[end]) {
			end++
		}
		if end > start + '_v3_lit_'.len
			&& (end == line.len || !c_generated_identifier_byte(line[end])) {
			symbols << line[start..end]
		}
		i = end
	}
	return symbols
}

fn c_generated_identifier_byte(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`
}

struct CDeclarationSection {
	begin string
	end   string
	keep  bool
}

fn c_native_declaration_directives(source string) string {
	mut out := strings.new_builder(source.len)
	mut in_block_comment := false
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		line_starts_in_comment := in_block_comment
		_, _, next_block_comment, _, _ := c_line_braces(line, in_block_comment)
		in_block_comment = next_block_comment
		if !line_starts_in_comment && trimmed.starts_with('#define ') {
			fields := trimmed['#define '.len..].fields()
			name := if fields.len > 0 { fields[0] } else { '' }
			if name.ends_with('_IMPLEMENTATION')
				|| (name.starts_with('SOKOL') && name.ends_with('_IMPL')) {
				out.writeln('/* v3 cache omitted ${name} */')
				continue
			}
		}
		out.writeln(line)
	}
	return c_native_localize_function_definitions(out.str())
}

fn c_native_localize_function_definitions(source string) string {
	mut out := strings.new_builder(source.len + 256)
	mut pending := strings.new_builder(256)
	mut brace_depth := 0
	mut in_block_comment := false
	for raw_line in source.split_into_lines() {
		delta, _, next_comment, last_code, first_open := c_line_braces(raw_line, in_block_comment)
		trimmed := raw_line.trim_space()
		if brace_depth == 0 {
			if pending.len == 0
				&& (in_block_comment || trimmed.len == 0 || trimmed.starts_with('//')
				|| trimmed.starts_with('#')
				|| trim_leading_c_comments(trimmed).len == 0) {
				out.writeln(raw_line)
				in_block_comment = next_comment
				continue
			}
			pending.writeln(raw_line)
			if first_open >= 0 {
				declaration := pending.str()
				current_line_start := declaration.len - raw_line.len - 1
				head :=
					trim_leading_c_comments(declaration[..current_line_start + first_open].trim_space())
				if c_static_declaration_head_is_function(head)
					&& !c_declaration_head_keeps_definition(head) {
					indent_len := declaration.len - declaration.trim_left(' \t').len
					out.write_string('${declaration[..indent_len]}static ${declaration[indent_len..]}')
				} else {
					out.write_string(declaration)
				}
				brace_depth += delta
				in_block_comment = next_comment
				continue
			}
			if last_code == `;` {
				out.write_string(pending.str())
			}
			in_block_comment = next_comment
			continue
		}
		out.writeln(raw_line)
		brace_depth += delta
		in_block_comment = next_comment
	}
	if pending.len > 0 {
		out.write_string(pending.str())
	}
	return out.str()
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
	mut item_head := strings.new_builder(512)
	mut brace_depth := 0
	mut has_brace := false
	mut is_function_block := false
	mut function_has_conditionals := false
	mut function_conditional_depth := 0
	mut has_static_storage := false
	mut in_block_comment := false
	mut in_preprocessor_directive := false
	mut preprocessor_in_item := false
	mut in_extern_c_block := false
	lines := prefix.split_into_lines()
	for line_idx, raw_line in lines {
		line := raw_line + '\n'
		trimmed := raw_line.trim_space()
		if in_preprocessor_directive {
			if preprocessor_in_item {
				item.write_string(line)
			} else {
				out.write_string(line)
			}
			in_preprocessor_directive = c_preprocessor_line_continues(raw_line)
			if !in_preprocessor_directive {
				preprocessor_in_item = false
			}
			continue
		}
		is_extern_c_open := trimmed.starts_with('extern "C"') && trimmed.ends_with('{')
		is_extern_c_close := in_extern_c_block && (trimmed == '}' || trimmed.starts_with('} //')
			|| trimmed.starts_with('} /*'))
		if !in_block_comment && brace_depth == 0 && (is_extern_c_open || is_extern_c_close) {
			if item.len > 0 {
				pending := item.str()
				if trim_leading_c_comments(pending).len != 0 {
					item.write_string(line)
					continue
				}
				out.write_string(pending)
				item_head.clear()
			}
			out.write_string(line)
			in_extern_c_block = is_extern_c_open
			continue
		}
		if !in_block_comment && trimmed.starts_with('#') {
			if brace_depth > 0 {
				if is_function_block {
					conditional_delta := c_preprocessor_conditional_delta(trimmed)
					if conditional_delta > 0 {
						function_has_conditionals = true
						function_conditional_depth++
					} else if conditional_delta < 0 && function_conditional_depth > 0 {
						function_conditional_depth--
					}
				}
				item.write_string(line)
				in_preprocessor_directive = c_preprocessor_line_continues(raw_line)
				preprocessor_in_item = in_preprocessor_directive
				continue
			}
			if brace_depth == 0 && item.len > 0 {
				pending := item.str()
				if trim_leading_c_comments(pending).len == 0 {
					out.write_string(pending)
					item_head.clear()
					has_brace = false
				} else {
					item.write_string(pending)
					item.write_string(line)
					in_preprocessor_directive = c_preprocessor_line_continues(raw_line)
					preprocessor_in_item = in_preprocessor_directive
					continue
				}
			}
			out.write_string(line)
			in_preprocessor_directive = c_preprocessor_line_continues(raw_line)
			continue
		}
		if !in_block_comment && brace_depth == 0 && item.len == 0
			&& (trimmed.len == 0 || trimmed.starts_with('//')) {
			out.write_string(line)
			continue
		}
		item.write_string(line)
		delta, saw_brace, next_comment, last_code, first_open := c_line_braces(raw_line,
			in_block_comment)
		in_block_comment = next_comment
		brace_depth += delta
		if !has_brace {
			if first_open >= 0 {
				item_head.write_string(raw_line[..first_open])
				head := item_head.str()
				clean_head := trim_leading_c_comments(head.trim_space())
				is_function_block = c_static_declaration_head_is_function(clean_head)
				function_has_conditionals = false
				function_conditional_depth = 0
			} else {
				item_head.write_string(line)
			}
			has_brace = saw_brace
		}
		mut closes_function_block := is_function_block && brace_depth <= 0
		if is_function_block && brace_depth > 0 && function_has_conditionals
			&& function_conditional_depth == 0 && c_function_block_closes_at_line_start(raw_line)
			&& c_next_line_is_preprocessor_endif(lines, line_idx + 1) {
			// C macro functions and conditionally compiled bodies do not always
			// have balanced raw braces. Once their conditional nesting has closed,
			// the column-zero outer delimiter still bounds the declaration.
			brace_depth = 0
			closes_function_block = true
		}
		if in_block_comment || brace_depth > 0 {
			continue
		}
		if has_brace {
			// Function and type blocks emitted by v3 finish at depth zero. A
			// typedef/global initializer may carry its semicolon on the same line.
			if !closes_function_block && last_code != `}` && last_code != `;` {
				continue
			}
		} else if last_code != `;` {
			continue
		}
		declaration := item.str()
		has_static_storage = has_static_storage
			|| c_declaration_item_has_static_storage(declaration, has_brace)
		out.write_string(c_declaration_item(declaration, has_brace))
		item_head.clear()
		brace_depth = 0
		has_brace = false
		is_function_block = false
		function_has_conditionals = false
		function_conditional_depth = 0
	}
	if item.len > 0 {
		declaration := item.str()
		has_static_storage = has_static_storage
			|| c_declaration_item_has_static_storage(declaration, has_brace)
		out.write_string(c_declaration_item(declaration, has_brace))
	}
	return out.str(), has_static_storage
}

fn c_function_block_closes_at_line_start(line string) bool {
	return line.len > 0 && line[0] == `}`
}

fn c_next_line_is_preprocessor_endif(lines []string, start int) bool {
	for line in lines[start..] {
		clean := line.trim_space()
		if clean.len == 0 || clean.starts_with('//') {
			continue
		}
		if clean.len < 2 || clean[0] != `#` {
			return false
		}
		fields := clean[1..].trim_space().fields()
		return fields.len > 0 && fields[0] == 'endif'
	}
	return false
}

fn c_preprocessor_conditional_delta(line string) int {
	clean := line.trim_space()
	if clean.len < 2 || clean[0] != `#` {
		return 0
	}
	fields := clean[1..].trim_space().fields()
	if fields.len == 0 {
		return 0
	}
	return match fields[0] {
		'if', 'ifdef', 'ifndef' { 1 }
		'endif' { -1 }
		else { 0 }
	}
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
			if c_static_declaration_head_is_function(head) {
				if c_has_static_storage_class(head) && !c_declaration_head_is_inline(head) {
					return true
				}
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
	return value.contains('(') && !c_contains_top_level_parenthesized_pointer_declarator(value)
		&& !c_contains_declaration_attribute(value) && !c_declaration_head_is_control_flow(value)
		&& !c_has_top_level_assign(value)
}

fn c_static_declaration_head_is_function(value string) bool {
	return value.contains('(') && !c_contains_top_level_parenthesized_pointer_declarator(value)
		&& !c_declaration_head_is_control_flow(value) && !c_has_top_level_assign(value)
}

fn c_declaration_head_is_control_flow(value string) bool {
	clean := trim_leading_c_comments(value.trim_space())
	for keyword in ['if', 'for', 'while', 'switch'] {
		if clean.starts_with('${keyword}(') || clean.starts_with('${keyword} (') {
			return true
		}
	}
	return false
}

fn c_declaration_head_keeps_definition(value string) bool {
	if c_has_static_storage_class(value) {
		return true
	}
	return !c_code_contains_identifier(value, 'extern') && c_declaration_head_is_inline(value)
}

fn c_declaration_head_is_inline(value string) bool {
	return c_code_contains_identifier(value, 'inline')
		|| c_code_contains_identifier(value, '__inline')
		|| c_code_contains_identifier(value, '__inline__')
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

fn c_contains_top_level_parenthesized_pointer_declarator(value string) bool {
	mut depth := 0
	for i, c in value.bytes() {
		if c == `)` {
			if depth > 0 {
				depth--
			}
			continue
		}
		if c != `(` {
			continue
		}
		is_top_level := depth == 0
		depth++
		if !is_top_level {
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

fn c_line_braces(line string, initial_block_comment bool) (int, bool, bool, u8, int) {
	mut depth := 0
	mut saw := false
	mut last_code := u8(0)
	mut first_open := -1
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
			if c !in [` `, `\t`, `\r`, `\n`] {
				last_code = c
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
			last_code = c
			i++
			continue
		}
		if c == `{` {
			if first_open < 0 {
				first_open = i
			}
			depth++
			saw = true
		} else if c == `}` {
			depth--
			saw = true
		}
		if c !in [` `, `\t`, `\r`, `\n`] {
			last_code = c
		}
		i++
	}
	return depth, saw, block_comment, last_code, first_open
}

// module_header serializes the declaration-only interface for one flat-AST module.
pub fn module_header(a &flat.FlatAst, tc &types.TypeChecker, module_name string, vroot string, import_paths map[string]string) string {
	mut out := strings.new_builder(4096)
	out.writeln('module ${module_name.all_after_last('.')}')
	generic_specialization_callees := generic_specialization_callee_names(tc)
	needs_source_bodies := module_needs_source_bodies(a, tc, module_name,
		generic_specialization_callees)
	mut source_cache := map[string]string{}
	embed_source_bodies := needs_source_bodies
		&& module_source_bodies_are_embeddable(a, tc, module_name, generic_specialization_callees, mut source_cache)
	if needs_source_bodies && !embed_source_bodies {
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
				needs_declaration_source := declaration_node_needs_source(a, id)
					|| node_creates_generic_specialization(a, tc, id, generic_specialization_callees)
				source_embedded := embed_source_bodies && needs_declaration_source
					&& declaration_node_source_is_embeddable(a, id)
				source_attrs_text := declaration_source_attrs_text(a, node, file_node.value, mut
					source_cache)
				source_is_public := declaration_source_is_public(a, node, file_node.value, mut
					source_cache)
				mut effective_attrs := attrs.attrs.clone()
				for source_attr in declaration_source_attr_values(source_attrs_text) {
					if source_attr !in effective_attrs {
						effective_attrs << source_attr
					}
				}
				mut text := if source_embedded {
					raw_source := declaration_source_with_line(a, node, file_node.value, mut
						source_cache) or { CachedDeclarationSource{} }
					cached_embedded_declaration_source(raw_source.text, vroot, file_node.value,
						raw_source.line)
				} else {
					decl_text(a, tc, module_name, node, vroot, file_node.value, import_paths,
						effective_attrs, source_is_public)
				}
				if text.len == 0 {
					continue
				}
				// Prefer the declaration's source spelling. Attribute marker node ids are
				// internal parser bookkeeping and can differ when a cache warmup replaces
				// some source files with headers; the source text is stable across both
				// layouts and prevents needless dependent-object invalidation.
				mut attrs_text := source_attrs_text
				if attrs_text.len == 0 {
					attrs_text = cached_declaration_attrs_text(attrs)
				}
				if attrs_text.len > 0 && (!source_embedded || !text.trim_space().starts_with('@[')) {
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
		params := node.generic_params()
		if decl_id < 0 || decl_id >= a.nodes.len || params.len == 0 {
			continue
		}
		result[decl_id] = CachedDeclarationAttrs{
			attrs: params.clone()
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

fn declaration_source_attrs_text(a &flat.FlatAst, node flat.Node, file_name string, mut source_cache map[string]string) string {
	text := declaration_source_text(a, node, file_name, mut source_cache) or { return '' }
	mut attrs := []string{}
	for line in text.split_into_lines() {
		clean := line.trim_space()
		if !clean.starts_with('@[') || !clean.ends_with(']') {
			break
		}
		attrs << clean
	}
	return attrs.join('\n')
}

fn declaration_source_attr_values(attrs_text string) []string {
	mut attrs := []string{}
	for line in attrs_text.split_into_lines() {
		clean := line.trim_space()
		if !clean.starts_with('@[') || !clean.ends_with(']') {
			continue
		}
		inner := clean[2..clean.len - 1]
		mut start := 0
		mut quote := u8(0)
		mut escaped := false
		for i, c in inner.bytes() {
			if escaped {
				escaped = false
				continue
			}
			if quote != 0 {
				if c == `\\` {
					escaped = true
				} else if c == quote {
					quote = 0
				}
				continue
			}
			if c in [`'`, `"`] {
				quote = c
				continue
			}
			if c == `;` {
				attr := inner[start..i].trim_space()
				if attr.len > 0 {
					attrs << attr
				}
				start = i + 1
			}
		}
		attr := inner[start..].trim_space()
		if attr.len > 0 {
			attrs << attr
		}
	}
	return attrs
}

fn declaration_source_is_public(a &flat.FlatAst, node flat.Node, file_name string, mut source_cache map[string]string) bool {
	text := declaration_source_text(a, node, file_name, mut source_cache) or { return false }
	for line in text.split_into_lines() {
		clean := line.trim_space()
		if clean.len == 0 || clean.starts_with('@[') {
			continue
		}
		return clean.starts_with('pub ')
	}
	return false
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

fn module_needs_source_bodies(a &flat.FlatAst, tc &types.TypeChecker, module_name string, generic_callees map[string]bool) bool {
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
				|| node_creates_generic_specialization(a, tc, id, generic_callees) {
				return true
			}
		}
	}
	return false
}

fn module_source_bodies_are_embeddable(a &flat.FlatAst, tc &types.TypeChecker, module_name string, generic_callees map[string]bool, mut source_cache map[string]string) bool {
	for file_node in a.nodes {
		if file_node.kind != .file || file_node.children_count == 0
			|| file_module_name(a, file_node) != module_name {
			continue
		}
		for i in 0 .. file_node.children_count {
			mut decl_ids := []flat.NodeId{}
			append_declaration_nodes(a, a.child(&file_node, i), mut decl_ids)
			for id in decl_ids {
				needs_source := declaration_node_needs_source(a, id)
					|| node_creates_generic_specialization(a, tc, id, generic_callees)
				if !needs_source {
					continue
				}
				node := a.nodes[int(id)]
				if !declaration_node_source_is_embeddable(a, id) {
					return false
				}
				source := declaration_source_text(a, node, file_node.value, mut source_cache) or {
					return false
				}
				if source.contains('@LOCATION') || source.contains('@COLUMN')
					|| source.contains('@VMODHASH') {
					return false
				}
			}
		}
	}
	return true
}

fn declaration_node_source_is_embeddable(a &flat.FlatAst, id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return false
	}
	node := a.nodes[int(id)]
	return node.kind in [.fn_decl, .const_decl, .struct_decl, .enum_decl, .interface_decl, .type_decl,
		.global_decl, .comptime_if]
}

struct CachedDeclarationSource {
	text string
	line int
}

fn declaration_source_text(a &flat.FlatAst, node flat.Node, file_name string, mut source_cache map[string]string) ?string {
	source := declaration_source_with_line(a, node, file_name, mut source_cache)?
	return source.text
}

fn declaration_source_with_line(a &flat.FlatAst, node flat.Node, file_name string, mut source_cache map[string]string) ?CachedDeclarationSource {
	if !node.pos.is_valid() || node.pos.offset < 0 {
		return none
	}
	path := if file_name.len > 0 {
		file_name
	} else {
		file := a.source_files[node.pos.id] or { return none }
		file.name
	}
	source := source_cache[path] or {
		loaded := os.read_file(path) or { return none }
		source_cache[path] = loaded
		loaded
	}
	start := declaration_source_start(a, source, node, node.pos.offset) or { return none }
	end := declaration_source_end(source, node.kind, start, declaration_descendant_end(a, node))
	if end <= start || end > source.len {
		return none
	}
	text := source[start..end]
	return CachedDeclarationSource{
		text: text
		line: if text.contains('@LINE') || text.contains('@FILE_LINE') {
			source[..start].count('\n') + 1
		} else {
			1
		}
	}
}

struct CachedSourcePathEdit {
	start       int
	end         int
	replacement string
}

fn cached_embedded_declaration_source(source string, vroot string, source_file string, source_line int) string {
	return cached_embedded_source_paths(source, vroot, source_file, source_line)
}

fn cached_embedded_directive_edit(source string, start int, vroot string, source_file string) ?CachedSourcePathEdit {
	mut line_start := start
	for line_start > 0 && source[line_start - 1] != `\n` {
		line_start--
	}
	for i in line_start .. start {
		if source[i] !in [` `, `\t`] {
			return none
		}
	}
	mut line_end := start + 1
	for line_end < source.len && source[line_end] != `\n` {
		line_end++
	}
	mut name_start := start + 1
	for name_start < line_end && source[name_start] in [` `, `\t`] {
		name_start++
	}
	mut name_end := name_start
	for name_end < line_end && ((source[name_end] >= `a` && source[name_end] <= `z`)
		|| source[name_end] == `_`) {
		name_end++
	}
	if name_end == name_start {
		return none
	}
	mut value_start := name_end
	for value_start < line_end && source[value_start] in [` `, `\t`] {
		value_start++
	}
	if value_start >= line_end {
		return none
	}
	directive := source[name_start..name_end]
	value := source[value_start..line_end]
	resolved := cached_directive_value(directive, value, vroot, source_file)
	if resolved == value {
		return none
	}
	return CachedSourcePathEdit{
		start:       value_start
		end:         line_end
		replacement: resolved
	}
}

fn cached_embedded_source_paths(source string, vroot string, source_file string, source_line int) string {
	mut out := strings.new_builder(source.len + 128)
	mut last := 0
	mut i := 0
	mut quote := u8(0)
	mut raw_string := false
	mut escaped := false
	mut line_comment := false
	mut block_comment := false
	mut line_nr := source_line
	for i < source.len {
		c := source[i]
		next := if i + 1 < source.len { source[i + 1] } else { u8(0) }
		if c == `\n` {
			line_nr++
		}
		if line_comment {
			if c == `\n` {
				line_comment = false
			}
			i++
			continue
		}
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
			if !raw_string && escaped {
				escaped = false
			} else if !raw_string && c == `\\` {
				escaped = true
			} else if c == quote {
				quote = 0
				raw_string = false
			}
			i++
			continue
		}
		if c == `/` && next == `/` {
			line_comment = true
			i += 2
			continue
		}
		if c == `/` && next == `*` {
			block_comment = true
			i += 2
			continue
		}
		if c == `#` {
			if edit := cached_embedded_directive_edit(source, i, vroot, source_file) {
				out.write_string(source[last..edit.start])
				out.write_string(edit.replacement)
				last = edit.end
				i = edit.end
				continue
			}
		}
		if c in [`'`, `"`] {
			quote = c
			raw_string = i > 0 && source[i - 1] == `r` && (i < 2 || (!source[i - 2].is_alnum()
				&& source[i - 2] != `_`))
			i++
			continue
		}
		if c == `$` && i + 1 < source.len && source[i + 1..].starts_with('embed_file') {
			if edit := cached_embed_file_path_edit(source, i, vroot, source_file) {
				out.write_string(source[last..edit.start])
				out.write_string(edit.replacement)
				last = edit.end
				i = edit.end
				continue
			}
		}
		if c == `@` {
			if edit := cached_source_pseudo_edit(source, i, source_file, line_nr) {
				out.write_string(source[last..edit.start])
				out.write_string(edit.replacement)
				last = edit.end
				i = edit.end
				continue
			}
		}
		i++
	}
	if last == 0 {
		return source
	}
	out.write_string(source[last..])
	return out.str()
}

fn cached_source_pseudo_edit(source string, start int, source_file string, line_nr int) ?CachedSourcePathEdit {
	mut name := ''
	for candidate in ['@VMOD_FILE', '@VMODROOT', '@FILE_LINE', '@FILE', '@DIR', '@LINE'] {
		if source[start..].starts_with(candidate) {
			name = candidate
			break
		}
	}
	if name.len == 0 {
		return none
	}
	end := start + name.len
	if end < source.len && (source[end].is_alnum() || source[end] == `_`) {
		return none
	}
	file := os.real_path(source_file)
	value := match name {
		'@VMOD_FILE' {
			_, vmod_file := signature_vmod_root(source_file)
			if vmod_file.len == 0 {
				return none
			}
			vmod_content := os.read_file(vmod_file) or { return none }
			vmod_content.replace('\r\n', '\n')
		}
		'@VMODROOT' {
			vmod_root, _ := signature_vmod_root(source_file)
			vmod_root
		}
		'@FILE_LINE' {
			'${file}:${line_nr}'
		}
		'@FILE' {
			file
		}
		'@DIR' {
			os.real_path(os.dir(source_file))
		}
		else {
			line_nr.str()
		}
	}
	return CachedSourcePathEdit{
		start:       start
		end:         end
		replacement: "'${escape_v_string(value)}'"
	}
}

fn cached_embed_file_path_edit(source string, start int, vroot string, source_file string) ?CachedSourcePathEdit {
	mut pos := start + 1 + 'embed_file'.len
	if pos > source.len || (pos < source.len && (source[pos].is_alnum() || source[pos] == `_`)) {
		return none
	}
	for pos < source.len && source[pos].is_space() {
		pos++
	}
	if pos >= source.len || source[pos] != `(` {
		return none
	}
	pos++
	for pos < source.len && source[pos].is_space() {
		pos++
	}
	argument_start := pos
	if pos + '@FILE'.len <= source.len && source[pos..].starts_with('@FILE') {
		end := pos + '@FILE'.len
		if end < source.len && (source[end].is_alnum() || source[end] == `_`) {
			return none
		}
		path := os.real_path(source_file)
		return CachedSourcePathEdit{
			start:       argument_start
			end:         end
			replacement: "'${escape_v_string(path)}'"
		}
	}
	mut is_raw := false
	if pos + 1 < source.len && source[pos] == `r` && source[pos + 1] in [`'`, `"`] {
		is_raw = true
		pos++
	}
	if pos >= source.len || source[pos] !in [`'`, `"`] {
		return none
	}
	quote := source[pos]
	content_start := pos + 1
	mut content_end := content_start
	for content_end < source.len {
		if !is_raw && source[content_end] == `\\` {
			if content_end + 1 >= source.len {
				return none
			}
			content_end += 2
			continue
		}
		if source[content_end] == quote {
			break
		}
		content_end++
	}
	if content_end >= source.len {
		return none
	}
	raw_path := source[content_start..content_end]
	path_value := if is_raw { raw_path } else { cached_unescape_v_string(raw_path) }
	path := cached_resolve_embedded_source_path(path_value, vroot, source_file) or { return none }
	return CachedSourcePathEdit{
		start:       argument_start
		end:         content_end + 1
		replacement: "'${escape_v_string(path)}'"
	}
}

fn cached_unescape_v_string(value string) string {
	if !value.contains('\\') {
		return value
	}
	mut out := strings.new_builder(value.len)
	mut i := 0
	for i < value.len {
		if value[i] != `\\` || i + 1 >= value.len {
			out.write_u8(value[i])
			i++
			continue
		}
		next := value[i + 1]
		if next == `\n` {
			i += 2
			for i < value.len && value[i] in [` `, `\t`, `\r`] {
				i++
			}
			continue
		}
		if next == `\r` && i + 2 < value.len && value[i + 2] == `\n` {
			i += 3
			for i < value.len && value[i] in [` `, `\t`] {
				i++
			}
			continue
		}
		if next == `x` && i + 3 < value.len {
			if code := cached_v_string_fixed_hex(value, i + 2, 2) {
				out.write_u8(u8(code))
				i += 4
				continue
			}
		}
		if next == `u` && i + 5 < value.len {
			if code := cached_v_string_fixed_hex(value, i + 2, 4) {
				out.write_rune(rune(code))
				i += 6
				continue
			}
		}
		if next == `U` && i + 9 < value.len {
			if code := cached_v_string_fixed_hex(value, i + 2, 8) {
				out.write_rune(rune(code))
				i += 10
				continue
			}
		}
		decoded := match next {
			`n` { int(`\n`) }
			`t` { int(`\t`) }
			`r` { int(`\r`) }
			`\\` { int(`\\`) }
			`'` { int(`'`) }
			`"` { int(`"`) }
			`$` { int(`$`) }
			`0` { 0 }
			`a` { 7 }
			`b` { 8 }
			`f` { 12 }
			`v` { 11 }
			else { -1 }
		}
		if decoded >= 0 {
			out.write_u8(u8(decoded))
		} else {
			out.write_u8(`\\`)
			out.write_u8(next)
		}
		i += 2
	}
	return out.str()
}

fn cached_v_string_fixed_hex(value string, start int, count int) ?u32 {
	mut code := u32(0)
	for i in 0 .. count {
		if start + i >= value.len {
			return none
		}
		digit := cached_v_string_hex_digit(value[start + i]) or { return none }
		code = (code << 4) | digit
	}
	return code
}

fn cached_v_string_hex_digit(c u8) ?u32 {
	if c >= `0` && c <= `9` {
		return u32(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return u32(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return u32(c - `A` + 10)
	}
	return none
}

fn cached_resolve_embedded_source_path(path string, vroot string, source_file string) ?string {
	if path.len == 0 || source_file.len == 0 {
		return none
	}
	mut resolved := path
	if resolved.starts_with('@VEXEROOT') && vroot.len > 0 {
		resolved = vroot + resolved['@VEXEROOT'.len..]
	}
	if resolved.starts_with('@VROOT') {
		resolved = '@VMODROOT' + resolved['@VROOT'.len..]
	}
	if resolved.starts_with('@VMODROOT') {
		resolved = cached_vmod_root(source_file) + resolved['@VMODROOT'.len..]
	}
	if !os.is_abs_path(resolved) {
		resolved = os.join_path_single(os.dir(source_file), resolved)
	}
	if !os.exists(resolved) {
		return none
	}
	return os.real_path(resolved)
}

fn declaration_source_start(a &flat.FlatAst, source string, node flat.Node, anchor int) ?int {
	if anchor < 0 || anchor > source.len {
		return none
	}
	if node.kind in [.const_decl, .global_decl] {
		mut line_start := 0
		for line_start < source.len {
			line_end := source.index_after('\n', line_start) or { source.len }
			line := source[line_start..line_end].trim_space()
			for i in 0 .. node.children_count {
				name := a.child_node(&node, i).value
				if name.len > 0 && declaration_source_named_line_matches(line, node.kind, name) {
					return declaration_source_attribute_start(source, line_start)
				}
			}
			if line_end >= source.len {
				break
			}
			line_start = line_end + 1
		}
	}
	mut pos := if anchor == source.len && anchor > 0 { anchor - 1 } else { anchor }
	for {
		mut start := pos
		for start > 0 && source[start - 1] != `\n` {
			start--
		}
		mut end := pos
		for end < source.len && source[end] != `\n` {
			end++
		}
		line := source[start..end].trim_space()
		if declaration_source_line_matches(line, node.kind) {
			return declaration_source_attribute_start(source, start)
		}
		if start == 0 {
			break
		}
		pos = start - 1
	}
	return none
}

fn declaration_source_attribute_start(source string, declaration_start int) int {
	mut start := declaration_start
	for start > 0 {
		mut line_end := start - 1
		if source[line_end] == `\r` {
			line_end--
		}
		mut line_start := line_end
		for line_start > 0 && source[line_start - 1] != `\n` {
			line_start--
		}
		line := source[line_start..line_end + 1].trim_space()
		if !line.starts_with('@[') || !line.ends_with(']') {
			break
		}
		start = line_start
	}
	return start
}

fn declaration_source_named_line_matches(line string, kind flat.NodeKind, name string) bool {
	return match kind {
		.const_decl {
			line.starts_with('const ${name} ') || line.starts_with('const ${name}=')
				|| line.starts_with('pub const ${name} ') || line.starts_with('pub const ${name}=')
		}
		.global_decl {
			line.starts_with('__global ${name} ') || line.starts_with('__global ${name}=')
		}
		else {
			false
		}
	}
}

fn declaration_source_line_matches(line string, kind flat.NodeKind) bool {
	clean := line.trim_left(' \t')
	return match kind {
		.fn_decl {
			clean.starts_with('fn ') || clean.starts_with('pub fn ')
		}
		.struct_decl {
			clean.starts_with('struct ') || clean.starts_with('pub struct ')
				|| clean.starts_with('union ') || clean.starts_with('pub union ')
		}
		.enum_decl {
			clean.starts_with('enum ') || clean.starts_with('pub enum ')
		}
		.interface_decl {
			clean.starts_with('interface ') || clean.starts_with('pub interface ')
		}
		.type_decl {
			clean.starts_with('type ') || clean.starts_with('pub type ')
		}
		.const_decl {
			clean.starts_with('const ') || clean.starts_with('pub const ')
		}
		.global_decl {
			clean.starts_with('__global')
		}
		.comptime_if {
			clean.starts_with('$if ')
		}
		else {
			false
		}
	}
}

fn declaration_descendant_end(a &flat.FlatAst, node flat.Node) int {
	mut end := if node.pos.end > node.pos.offset { node.pos.end } else { node.pos.offset }
	for i in 0 .. node.children_count {
		child := a.child_node(&node, i)
		child_end := declaration_descendant_end(a, *child)
		if child_end > end {
			end = child_end
		}
	}
	return end
}

fn declaration_source_end(source string, kind flat.NodeKind, start int, descendant_end int) int {
	if kind in [.fn_decl, .struct_decl, .enum_decl, .interface_decl, .comptime_if] {
		if end := source_balanced_declaration_end(source, start, `{`, `}`) {
			return end
		}
	}
	if kind in [.const_decl, .global_decl] {
		if end := source_value_declaration_end(source, kind, start) {
			return end
		}
	}
	line_end := source.index_after('\n', start) or { source.len }
	mut end := if descendant_end > start { descendant_end } else { line_end }
	for end < source.len && source[end] != `\n` {
		end++
	}
	return end
}

fn source_value_declaration_end(source string, kind flat.NodeKind, start int) ?int {
	mut quote := u8(0)
	mut escaped := false
	mut line_comment := false
	mut block_comment := false
	mut paren_depth := 0
	mut bracket_depth := 0
	mut brace_depth := 0
	mut last_code := u8(0)
	mut line_start := start
	mut saw_declaration := false
	mut i := start
	for i < source.len {
		c := source[i]
		next := if i + 1 < source.len { source[i + 1] } else { u8(0) }
		if line_comment {
			if c == `\n` {
				line_comment = false
				saw_declaration = saw_declaration
					|| declaration_source_line_matches(source[line_start..i].trim_space(), kind)
				if saw_declaration && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0
					&& !source_value_line_continues(last_code) {
					return i
				}
				line_start = i + 1
			}
			i++
			continue
		}
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
			line_comment = true
			i += 2
			continue
		}
		if c == `/` && next == `*` {
			block_comment = true
			i += 2
			continue
		}
		if c in [`'`, `"`, `\``] {
			quote = c
			last_code = c
			i++
			continue
		}
		match c {
			`(` { paren_depth++ }
			`)` { paren_depth-- }
			`[` { bracket_depth++ }
			`]` { bracket_depth-- }
			`{` { brace_depth++ }
			`}` { brace_depth-- }
			else {}
		}
		if c == `\n` {
			saw_declaration = saw_declaration
				|| declaration_source_line_matches(source[line_start..i].trim_space(), kind)
			if saw_declaration && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0
				&& !source_value_line_continues(last_code) {
				return i
			}
			line_start = i + 1
		} else if c !in [` `, `\t`, `\r`] {
			last_code = c
		}
		i++
	}
	if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
		return source.len
	}
	return none
}

fn source_value_line_continues(last_code u8) bool {
	return last_code in [`=`, `,`, `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `.`, `?`, `:`, `\\`]
}

fn source_balanced_declaration_end(source string, start int, open u8, close u8) ?int {
	mut quote := u8(0)
	mut escaped := false
	mut line_comment := false
	mut block_comment := false
	mut depth := 0
	mut found := false
	mut i := start
	for i < source.len {
		c := source[i]
		next := if i + 1 < source.len { source[i + 1] } else { u8(0) }
		if line_comment {
			if c == `\n` {
				line_comment = false
			}
			i++
			continue
		}
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
			line_comment = true
			i += 2
			continue
		}
		if c == `/` && next == `*` {
			block_comment = true
			i += 2
			continue
		}
		if c in [`'`, `"`, `\``] {
			quote = c
			i++
			continue
		}
		if c == open {
			depth++
			found = true
		} else if c == close && found {
			depth--
			if depth == 0 {
				return i + 1
			}
		}
		i++
	}
	return none
}

fn node_creates_generic_specialization(a &flat.FlatAst, tc &types.TypeChecker, id flat.NodeId, generic_callees map[string]bool) bool {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return false
	}
	if name := tc.resolved_call_name(id) {
		if name in tc.fn_generic_params || generic_callees[name] {
			return true
		}
	}
	if name := tc.resolved_fn_value_name(id) {
		if name in tc.fn_generic_params || generic_callees[name] {
			return true
		}
	}
	node := a.nodes[int(id)]
	if node.kind == .call && node.children_count > 0 {
		callee_id := a.child(&node, 0)
		if name := generic_call_source_name(a, callee_id) {
			if generic_callees[name] {
				return true
			}
		}
		callee := a.nodes[int(callee_id)]
		if callee.kind == .selector && callee.children_count > 0 {
			receiver_type := tc.resolve_type(a.child(callee, 0)).name().trim_left('&?')
			receiver_base := receiver_type.all_before('[')
			if receiver_type.contains('[') && (receiver_base in tc.struct_generic_params
				|| receiver_base.all_after_last('.') in tc.struct_generic_params) {
				return true
			}
		}
	}
	for i in 0 .. node.children_count {
		if node_creates_generic_specialization(a, tc, a.child(&node, i), generic_callees) {
			return true
		}
	}
	return false
}

fn generic_call_source_name(a &flat.FlatAst, id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return none
	}
	node := a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return none
			}
			base := a.child_node(&node, 0)
			if base.kind == .ident && base.value.len > 0 && node.value.len > 0 {
				return '${base.value}.${node.value}'
			}
		}
		.index, .paren {
			if node.children_count > 0 {
				return generic_call_source_name(a, a.child(&node, 0))
			}
		}
		else {}
	}
	return none
}

fn generic_specialization_callee_names(tc &types.TypeChecker) map[string]bool {
	mut names := map[string]bool{}
	for name in tc.fn_generic_params.keys() {
		names[name] = true
	}
	for name in tc.fn_param_type_texts.keys() {
		if !name.contains('[') {
			continue
		}
		closed := generic_receiver_name_without_type_args(name)
		if closed.len == 0 {
			continue
		}
		names[closed] = true
		receiver := closed.all_before_last('.')
		method := closed.all_after_last('.')
		if receiver.contains('.') {
			names['${receiver.all_after_last('.')}.${method}'] = true
		}
	}
	return names
}

fn generic_receiver_name_without_type_args(name string) string {
	mut out := strings.new_builder(name.len)
	mut depth := 0
	for c in name.bytes() {
		if c == `[` {
			depth++
			continue
		}
		if c == `]` {
			if depth > 0 {
				depth--
			}
			continue
		}
		if depth == 0 {
			out.write_u8(c)
		}
	}
	return out.str()
}

fn declaration_node_needs_source(a &flat.FlatAst, id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return false
	}
	node := a.nodes[int(id)]
	if node.generic_params().len > 0 || fn_decl_has_generic_receiver(a, node)
		|| declaration_contains_fn_literal(a, node)
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

fn declaration_contains_fn_literal(a &flat.FlatAst, node flat.Node) bool {
	if node.kind == .fn_literal {
		return true
	}
	for i in 0 .. node.children_count {
		if declaration_contains_fn_literal(a, *a.child_node(&node, i)) {
			return true
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

fn decl_text(a &flat.FlatAst, tc &types.TypeChecker, module_name string, node flat.Node, vroot string, source_file string, import_paths map[string]string, declaration_attrs []string, source_is_public bool) string {
	return match node.kind {
		.import_decl {
			import_text(a, node, import_paths)
		}
		.fn_decl {
			fn_text(a, module_name, node, false, declaration_attrs, source_is_public)
		}
		.c_fn_decl {
			fn_text(a, module_name, node, true, declaration_attrs, source_is_public)
		}
		.struct_decl {
			struct_text(a, node, declaration_attrs, source_is_public)
		}
		.global_decl {
			global_text(a, tc, module_name, node)
		}
		.const_decl {
			const_text(a, tc, node, source_is_public)
		}
		.enum_decl {
			enum_text(a, node, declaration_attrs, source_is_public)
		}
		.type_decl {
			type_text(a, node, source_is_public)
		}
		.interface_decl {
			interface_text(a, node, source_is_public)
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
	candidate := os.join_path_single(os.dir(source_file), path)
	if !os.exists(candidate) {
		return value
	}
	resolved := os.real_path(candidate)
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

fn fn_text(a &flat.FlatAst, module_name string, node flat.Node, is_c bool, declaration_attrs []string, source_is_public bool) string {
	mut params := []flat.Node{}
	for i in 0 .. node.children_count {
		child := a.child_node(&node, i)
		if child.kind == .param {
			params << child
		}
	}
	mut name := node.value
	visibility := if !is_c && (node.op == .arrow || source_is_public) { 'pub ' } else { '' }
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
	generic_params := node.generic_params()
	if generic_params.len > 0 {
		head += '[${generic_params.join(', ')}]'
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
		if p.is_mut && p.op == .amp {
			prefix = 'mut '
			if !ptype.starts_with('&') {
				ptype = '&${ptype}'
			}
		} else if ptype.starts_with('&') && p.is_mut {
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

fn struct_text(a &flat.FlatAst, node flat.Node, declaration_attrs []string, source_is_public bool) string {
	kind := if node.typ.split(',').contains('union') { 'union' } else { 'struct' }
	visibility := if source_is_public { 'pub ' } else { '' }
	mut head := '${visibility}${kind} ${node.value}${generic_suffix(node.generic_params())}'
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
		field_params := field.generic_params()
		flags := if field_params.len > 0 { field_params[0] } else { '' }
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
		for ai := 1; ai < field_params.len; ai++ {
			out.writeln('\t@[${field_params[ai]}]')
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
			line += ' ${cached_type_source_text(tc, field_type)}'
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

fn cached_type_source_text(tc &types.TypeChecker, type_text string) string {
	typ := tc.parse_resolution_type(type_text)
	if typ is types.ArrayFixed {
		len_text := if typ.len_expr.len > 0 { typ.len_expr } else { typ.len.str() }
		return '[${len_text}]${cached_type_source_name(typ.elem_type)}'
	}
	return type_text
}

fn cached_type_source_name(typ types.Type) string {
	if typ is types.ArrayFixed {
		len_text := if typ.len_expr.len > 0 { typ.len_expr } else { typ.len.str() }
		return '[${len_text}]${cached_type_source_name(typ.elem_type)}'
	}
	return typ.name()
}

fn const_text(a &flat.FlatAst, tc &types.TypeChecker, node flat.Node, source_is_public bool) string {
	mut lines := []string{cap: node.children_count}
	for i in 0 .. node.children_count {
		field := a.child_node(&node, i)
		mut line := field.value
		if field.children_count > 0 {
			expr_id := a.child(field, 0)
			mut value := expr_text(a, expr_id)
			expr := a.node(expr_id)
			if expr.kind == .array_literal {
				typ := tc.resolve_type(expr_id)
				if typ is types.Array {
					// Cache headers are declaration inputs. A bare const literal uses
					// fixed storage there and disagrees with the cached object's source ABI.
					value = '${value}.clone()'
				}
			}
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
		lines << line
	}
	if source_is_public && lines.len == 1 {
		return 'pub const ${lines[0]}'
	}
	mut out := strings.new_builder(128)
	visibility := if source_is_public { 'pub ' } else { '' }
	out.writeln('${visibility}const (')
	for line in lines {
		out.writeln('\t${line}')
	}
	out.write_string(')')
	return out.str()
}

fn enum_text(a &flat.FlatAst, node flat.Node, declaration_attrs []string, source_is_public bool) string {
	mut out := strings.new_builder(128)
	if node.typ == 'flag' && !cached_declaration_has_attr(declaration_attrs, 'flag') {
		out.writeln('@[flag]')
	}
	params := node.generic_params()
	if params.contains('json_as_number')
		&& !cached_declaration_has_attr(declaration_attrs, 'json_as_number') {
		out.writeln('@[json_as_number]')
	}
	visibility := if source_is_public { 'pub ' } else { '' }
	mut head := '${visibility}enum ${node.value}'
	if params.len > 0 && params[0].len > 0 {
		head += ' as ${params[0]}'
	}
	out.writeln('${head} {')
	for i in 0 .. node.children_count {
		field := a.child_node(&node, i)
		for attr in field.generic_params() {
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

fn type_text(a &flat.FlatAst, node flat.Node, source_is_public bool) string {
	visibility := if source_is_public { 'pub ' } else { '' }
	head := '${visibility}type ${node.value}${generic_suffix(node.generic_params())} = '
	if node.children_count == 0 {
		return head + node.typ
	}
	mut variants := []string{}
	for i in 0 .. node.children_count {
		variants << a.child_node(&node, i).value
	}
	return head + variants.join(' | ')
}

fn interface_text(a &flat.FlatAst, node flat.Node, source_is_public bool) string {
	mut out := strings.new_builder(128)
	visibility := if source_is_public { 'pub ' } else { '' }
	out.writeln('${visibility}interface ${node.value}${generic_suffix(node.generic_params())} {')
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
				value := expr_text(a, a.child(&node, 0))
				if node.value.len > 0 {
					'${node.value}: ${value}'
				} else {
					value
				}
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
	// The scanner keeps the original spelling between the backticks, including
	// escape sequences. Escaping it again would turn `\\` into `\\\\` in a
	// cached header and change the rune's value when that header is parsed.
	return value
}
