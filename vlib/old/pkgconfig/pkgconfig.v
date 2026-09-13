module pkgconfig

import semver
import os

const version = '0.3.4'

pub enum LinkMode {
	dynamic
	static_
}

const default_paths = [
	'/usr/local/lib/x86_64-linux-gnu/pkgconfig',
	'/usr/local/lib64/pkgconfig',
	'/usr/local/lib/pkgconfig',
	'/usr/local/share/pkgconfig',
	'/usr/lib/x86_64-linux-gnu/pkgconfig',
	'/usr/lib/aarch64-linux-gnu/pkgconfig',
	'/usr/lib64/pkgconfig',
	'/usr/lib/pkgconfig',
	'/usr/share/pkgconfig',
	'/opt/homebrew/lib/pkgconfig', // Brew on macOS
	'/opt/homebrew/share/pkgconfig', // Brew on macOS. Needed for fish.pc, eigen3.pc, applewmproto.pc, fontsproto.pc, xextproto.pc, SPIRV-Headers.pc etc; seems like a legacy folder.
	'/opt/homebrew/Library/Homebrew/os/mac/pkgconfig/11', // Brew on macOS. Needed for zlib.pc, libcurl.pc, expat.pc and a few others; all the rest are symlinked in /opt/homebrew/lib/pkgconfig .
	'/opt/local/lib/pkgconfig', // MacPorts on macOS
	'/usr/local/libdata/pkgconfig', // FreeBSD
	'/usr/libdata/pkgconfig', // FreeBSD
	'/usr/lib/i386-linux-gnu/pkgconfig', // Debian 32bit
	'/data/data/com.termux/files/usr/lib/pkgconfig', // Termux
	'/usr/pkg/lib/pkgconfig', // NetBSD
]

pub struct Options {
pub:
	path              string
	debug             bool
	norecurse         bool
	only_description  bool
	use_default_paths bool = true
	link_mode         LinkMode
}

pub struct ResolvedFlags {
pub:
	cflags []string
	libs   []string
}

pub struct PkgConfig {
pub mut:
	file_path        string
	options          Options
	name             string
	modname          string
	url              string
	version          string
	description      string
	libs             []string
	libs_private     []string
	cflags           []string
	cflags_private   []string
	paths            []string // TODO: move to options?
	vars             map[string]string
	requires         []string
	requires_private []string
	conflicts        []string
	loaded           []string
mut:
	define_prefix_source string
	define_prefix_target string
}

fn (mut pc PkgConfig) parse_list_no_comma(s string) []string {
	return pc.parse_list(s.replace(',', ' '))
}

fn (mut pc PkgConfig) parse_list(s string) []string {
	operators := ['=', '<', '>', '>=', '<=']
	value := pc.parse_line(s.replace('  ', ' ').replace(', ', ' '))
	r := split_list_with_protected_fragment(value, pc.define_prefix_target)
	mut res := []string{}
	mut skip := false
	for a in r {
		b := a.trim_space()
		if skip {
			skip = false
		} else if b in operators {
			skip = true
		} else if b != '' {
			res << b
		}
	}
	return res
}

fn split_list_with_protected_fragment(value string, protected_fragment string) []string {
	if protected_fragment == '' || !protected_fragment.contains(' ')
		|| !value.contains(protected_fragment) {
		return value.split(' ')
	}
	mut fragments := []string{}
	mut current := []u8{}
	mut i := 0
	for i < value.len {
		if value[i..].starts_with(protected_fragment) {
			current << protected_fragment.bytes()
			i += protected_fragment.len
			continue
		}
		if value[i] == ` ` {
			fragments << current.bytestr()
			current = []u8{}
			i++
			continue
		}
		current << value[i]
		i++
	}
	fragments << current.bytestr()
	return fragments
}

fn fold_static_continued_lines(data string) []string {
	physical_lines := data.split('\n')
	mut lines := []string{cap: physical_lines.len}
	mut continued := ''
	mut is_continuation := false
	for i, raw_line in physical_lines {
		line := if is_continuation { raw_line.trim_left(' \t') } else { raw_line }
		mut content_end := line.len
		if content_end > 0 && line[content_end - 1] == `\r` {
			content_end--
		}
		mut slash_start := content_end
		for slash_start > 0 && line[slash_start - 1] == `\\` {
			slash_start--
		}
		if (content_end - slash_start) % 2 == 1 {
			continued += line[..content_end - 1]
			if i + 1 < physical_lines.len {
				is_continuation = true
				continue
			}
			lines << continued
			continue
		}
		if is_continuation {
			continued += line
			lines << continued
			continued = ''
			is_continuation = false
		} else {
			lines << line
		}
	}
	return lines
}

fn static_link_field_without_comment(s string) string {
	mut preceding_backslashes := 0
	for i, ch in s {
		if ch == `\\` {
			preceding_backslashes++
			continue
		}
		if ch == `#` && preceding_backslashes % 2 == 0 {
			return s[..i]
		}
		preceding_backslashes = 0
	}
	return s
}

fn normalize_static_link_field(s string) string {
	mut normalized := []u8{cap: s.len}
	mut quote := u8(0)
	mut i := 0
	for i < s.len {
		ch := s[i]
		if ch == `\\` && quote != `'` && i + 1 < s.len {
			normalized << ch
			normalized << s[i + 1]
			i += 2
			continue
		}
		if ch in [`'`, `"`] {
			if quote == 0 {
				quote = ch
			} else if quote == ch {
				quote = 0
			}
			normalized << ch
			i++
			continue
		}
		if quote == 0 && ch == `,` && i + 1 < s.len && s[i + 1] == ` ` {
			normalized << ` `
			i += 2
			continue
		}
		if quote == 0 && ch == ` ` {
			normalized << ch
			for i + 1 < s.len && s[i + 1] == ` ` {
				i++
			}
			i++
			continue
		}
		normalized << ch
		i++
	}
	return normalized.bytestr()
}

fn tokenize_static_list_with_protected_fragment(value string, protected_fragment string) ![]string {
	mut fragments := []string{}
	mut current := []u8{}
	mut quote := u8(0)
	mut has_fragment := false
	mut i := 0
	for i < value.len {
		if protected_fragment != '' && value[i..].starts_with(protected_fragment) {
			current << protected_fragment.bytes()
			has_fragment = true
			i += protected_fragment.len
			continue
		}
		ch := value[i]
		if quote == 0 && ch in [` `, `\t`, `\r`, `\n`] {
			if has_fragment {
				fragments << current.bytestr()
				current = []u8{}
				has_fragment = false
			}
			i++
			continue
		}
		if ch in [`'`, `"`] {
			if quote == 0 {
				quote = ch
				has_fragment = true
				i++
				continue
			}
			if quote == ch {
				quote = 0
				i++
				continue
			}
		}
		if ch == `\\` && quote != `'` && i + 1 < value.len {
			next := value[i + 1]
			escapable := if quote == `"` {
				next in [`"`, `\\`, `#`]
			} else {
				next in [` `, `\t`, `\r`, `\n`, `'`, `"`, `\\`, `#`]
			}
			if escapable {
				current << next
				has_fragment = true
				i += 2
				continue
			}
		}
		current << ch
		has_fragment = true
		i++
	}
	if quote != 0 {
		return error('unterminated quote in static pkg-config linker flags')
	}
	if has_fragment {
		fragments << current.bytestr()
	}
	return fragments
}

fn tokenize_static_list(value string) ![]string {
	return tokenize_static_list_with_protected_fragment(value, '')
}

fn (mut pc PkgConfig) parse_static_link_list(s string) ![]string {
	raw_value := static_link_field_without_comment(normalize_static_link_field(s))
	value := pc.expand_static_link_variables(raw_value)
	return tokenize_static_list_with_protected_fragment(value, pc.define_prefix_target)
}

fn (mut pc PkgConfig) expand_static_link_variables(s string) string {
	mut r := s
	for r.contains('\${') {
		tok0 := r.index('\${') or { break }
		mut tok1 := r[tok0..].index('}') or { break }
		tok1 += tok0
		v := r[tok0 + 2..tok1]
		r = r.replace('\${${v}}', pc.vars[v])
	}
	return r.trim_space()
}

fn (mut pc PkgConfig) parse_line(s string) string {
	mut r := s.split('#')[0]
	for r.contains('\${') {
		tok0 := r.index('\${') or { break }
		mut tok1 := r[tok0..].index('}') or { break }
		tok1 += tok0
		v := r[tok0 + 2..tok1]
		r = r.replace('\${${v}}', pc.vars[v])
	}
	return r.trim_space()
}

fn trim_define_prefix_path(path string) string {
	mut end := path.len
	for end > 1 && path[end - 1] in [`/`, `\\`] {
		end--
	}
	return path[..end]
}

fn define_prefix_from_pcfiledir(pcfiledir string) ?string {
	if os.base(pcfiledir).to_lower_ascii() != 'pkgconfig' {
		return none
	}
	metadata_parent := os.dir(pcfiledir)
	if metadata_parent == '' || metadata_parent == '.' {
		return none
	}
	prefix := os.dir(metadata_parent)
	if prefix == '' || prefix == '.' {
		return none
	}
	return trim_define_prefix_path(prefix)
}

fn has_define_prefix_path(value string, prefix string) bool {
	if prefix == '' || value.len < prefix.len
		|| value[..prefix.len].to_lower_ascii() != prefix.to_lower_ascii() {
		return false
	}
	if value.len == prefix.len || prefix.ends_with('/') || prefix.ends_with('\\') {
		return true
	}
	return value[prefix.len] in [`/`, `\\`]
}

fn (pc &PkgConfig) relocate_define_prefix_value(value string) string {
	if !has_define_prefix_path(value, pc.define_prefix_source) {
		return value
	}
	return pc.define_prefix_target + value[pc.define_prefix_source.len..]
}

fn (mut pc PkgConfig) set_variable(key string, raw_value string) {
	parsed_value := pc.parse_line(raw_value)
	value := pc.parse_line(parsed_value)
	if key == 'prefix' {
		pc.define_prefix_source = ''
		pc.define_prefix_target = ''
		$if windows {
			source := trim_define_prefix_path(value)
			if source.starts_with('/') && !source.starts_with('//') {
				if target := define_prefix_from_pcfiledir(pc.vars['pcfiledir']) {
					pc.define_prefix_source = source
					pc.define_prefix_target = target
					pc.vars[key] = target
					return
				}
			}
		}
		pc.vars[key] = value
		return
	}
	pc.vars[key] = pc.relocate_define_prefix_value(value)
}

fn (mut pc PkgConfig) setvar(line string) {
	kv := line.trim_space().split('=')
	if kv.len == 2 {
		pc.set_variable(kv[0], kv[1])
	}
}

fn split_static_field(line string) (string, string, bool) {
	separator := line.index_u8(`:`)
	if separator < 0 {
		return '', '', false
	}
	return line[..separator].trim_space().to_lower(), line[separator + 1..], true
}

fn split_static_variable(line string) (string, string, bool) {
	separator := line.index_u8(`=`)
	if separator < 0 {
		return '', '', false
	}
	key := line[..separator].trim(' \t')
	if key == '' || key.contains_any(' \t\r\n') || key.contains(':') {
		return '', '', false
	}
	return key, line[separator + 1..].trim(' \t'), true
}

fn (mut pc PkgConfig) set_static_var(line string) bool {
	key, value, is_variable := split_static_variable(line)
	if !is_variable {
		return false
	}
	pc.set_variable(key, value)
	return true
}

fn (mut pc PkgConfig) parse_dynamic_fields(lines []string) {
	if pc.options.only_description {
		// Keep the historical dynamic parser byte-for-byte selective.
		for line in lines {
			if line.starts_with('Description: ') {
				pc.description = pc.parse_line(line[13..])
			}
		}
		return
	}
	for oline in lines {
		line := oline.trim_space()
		if line.starts_with('#') {
			continue
		}
		if line.contains('=') && !line.contains(' ') {
			pc.setvar(line)
			continue
		}
		if line.starts_with('Name:') {
			pc.name = pc.parse_line(line[5..])
		} else if line.starts_with('Description:') {
			pc.description = pc.parse_line(line[12..])
		} else if line.starts_with('Version:') {
			pc.version = pc.parse_line(line[8..])
		} else if line.starts_with('Requires:') {
			pc.requires = pc.parse_list_no_comma(line[9..])
		} else if line.starts_with('Requires.private:') {
			pc.requires_private = pc.parse_list_no_comma(line[17..])
		} else if line.starts_with('Conflicts:') {
			pc.conflicts = pc.parse_list_no_comma(line[10..])
		} else if line.starts_with('Cflags:') {
			pc.cflags = pc.parse_list(line[7..])
		} else if line.starts_with('Libs:') {
			pc.libs = pc.parse_list(line[5..])
		} else if line.starts_with('Libs.private:') {
			pc.libs_private = pc.parse_list(line[13..])
		} else if line.starts_with('URL:') {
			pc.url = pc.parse_line(line[4..])
		}
	}
}

fn (mut pc PkgConfig) parse_static_fields(lines []string) ! {
	if pc.options.only_description {
		for line in lines {
			key, value, is_field := split_static_field(line)
			if is_field && key == 'description' {
				pc.description = pc.parse_line(value)
			}
		}
		return
	}
	for oline in lines {
		line := oline.trim_space()
		if line.starts_with('#') {
			continue
		}
		if pc.set_static_var(line) {
			continue
		}
		key, value, is_field := split_static_field(line)
		if !is_field {
			continue
		}
		match key {
			'name' { pc.name = pc.parse_line(value) }
			'description' { pc.description = pc.parse_line(value) }
			'version' { pc.version = pc.parse_line(value) }
			'requires' { pc.requires << pc.parse_list_no_comma(value) }
			'requires.private' { pc.requires_private << pc.parse_list_no_comma(value) }
			'conflicts' { pc.conflicts = pc.parse_list_no_comma(value) }
			'cflags' { pc.cflags << pc.parse_list(value) }
			'cflags.private' { pc.cflags_private << pc.parse_list(value) }
			'libs' { pc.libs << pc.parse_static_link_list(value)! }
			'libs.private' { pc.libs_private << pc.parse_static_link_list(value)! }
			'url' { pc.url = pc.parse_line(value) }
			else {}
		}
	}
}

fn (mut pc PkgConfig) parse(file string) ! {
	pc.file_path = file
	pc.vars['pcfiledir'] = os.real_path(os.dir(file))
	data := os.read_file(file)!
	if pc.options.debug {
		eprintln(data)
	}
	if pc.options.link_mode == .static_ {
		pc.parse_static_fields(fold_static_continued_lines(data))!
	} else {
		pc.parse_dynamic_fields(data.split('\n'))
	}
}

fn (mut pc PkgConfig) resolve(pkgname string) !string {
	if pkgname.ends_with('.pc') {
		if os.exists(pkgname) {
			return pkgname
		}
	} else {
		if pc.paths.len == 0 {
			pc.paths << '.'
		}
		for path in pc.paths {
			file := '${path}/${pkgname}.pc'
			if os.exists(file) {
				return file
			}
		}
	}
	return error('Cannot find "${pkgname}" pkgconfig file')
}

pub fn atleast(v string) bool {
	v0 := semver.from(version) or { return false }
	v1 := semver.from(v) or { return false }
	return v0 > v1
}

pub fn (mut pc PkgConfig) atleast(v string) bool {
	v0 := semver.from(pc.version) or { return false }
	v1 := semver.from(v) or { return false }
	return v0 > v1
}

pub fn (mut pc PkgConfig) extend(pcdep &PkgConfig) string {
	for flag in pcdep.cflags {
		if pc.cflags.index(flag) == -1 {
			pc.cflags << flag
		}
	}
	for flag in pcdep.cflags_private {
		if pc.cflags_private.index(flag) == -1 {
			pc.cflags_private << flag
		}
	}
	for lib in pcdep.libs {
		if pc.libs.index(lib) == -1 {
			pc.libs << lib
		}
	}
	for lib in pcdep.libs_private {
		if pc.libs_private.index(lib) == -1 {
			pc.libs_private << lib
		}
	}
	return ''
}

fn (mut pc PkgConfig) load_requires() ! {
	for dep in pc.requires {
		pc.load_require(dep)!
	}
	for dep in pc.requires_private {
		pc.load_require(dep)!
	}
}

fn (mut pc PkgConfig) load_require(dep string) ! {
	if dep in pc.loaded {
		return
	}
	pc.loaded << dep
	mut pcdep := PkgConfig{
		paths:   pc.paths
		loaded:  pc.loaded
		options: Options{
			link_mode: pc.options.link_mode
		}
	}
	depfile := pcdep.resolve(dep) or {
		if pc.options.debug {
			eprintln('cannot resolve ${dep}')
		}
		return error('could not resolve dependency ${dep}')
	}
	pcdep.parse(depfile) or {
		return error('required file "${depfile}" could not be parsed: ${err.msg()}')
	}
	if !pc.options.norecurse {
		pcdep.load_requires()!
	}
	pc.extend(pcdep)
}

struct StaticResolver {
	options Options
mut:
	packages      map[string]&PkgConfig
	solve_state   map[string]u8
	blocked_edges map[string]bool
	visited       map[string]bool
	order         []string
}

fn (mut r StaticResolver) load_package(pkgname string) !&PkgConfig {
	if pkgname in r.packages {
		return r.packages[pkgname] or { return error('could not load package ${pkgname}') }
	}
	pc := load_direct(pkgname, r.options)!
	r.packages[pkgname] = pc
	return pc
}

fn static_dependency_edge(parent string, dependency string) string {
	return '${parent}\x00${dependency}'
}

fn (mut r StaticResolver) prepare(pkgname string) ! {
	if r.solve_state[pkgname] == 2 {
		return
	}
	pc := r.load_package(pkgname)!
	r.solve_state[pkgname] = 1
	if !r.options.norecurse {
		for dependency in pc.requires {
			if r.solve_state[dependency] == 1 {
				r.blocked_edges[static_dependency_edge(pkgname, dependency)] = true
				continue
			}
			r.prepare(dependency) or {
				if r.options.debug {
					eprintln('cannot resolve ${dependency}: ${err.msg()}')
				}
				return error('could not resolve dependency ${dependency}: ${err.msg()}')
			}
		}
		for dependency in pc.requires_private {
			if r.solve_state[dependency] == 1 {
				continue
			}
			r.prepare(dependency) or {
				if r.options.debug {
					eprintln('cannot resolve ${dependency}: ${err.msg()}')
				}
				return error('could not resolve dependency ${dependency}: ${err.msg()}')
			}
		}
	}
	r.solve_state[pkgname] = 2
}

fn (mut r StaticResolver) collect(pkgname string) ! {
	if r.visited[pkgname] {
		return
	}
	pc := r.load_package(pkgname)!
	r.visited[pkgname] = true
	if !r.options.norecurse {
		// pkgconf flattens private dependencies first, then public dependencies,
		// walking each list backwards and prepending each completed package.
		for i := pc.requires_private.len - 1; i >= 0; i-- {
			dependency := pc.requires_private[i]
			r.collect(dependency) or {
				if r.options.debug {
					eprintln('cannot resolve ${dependency}: ${err.msg()}')
				}
				return error('could not resolve dependency ${dependency}: ${err.msg()}')
			}
		}
		for i := pc.requires.len - 1; i >= 0; i-- {
			dependency := pc.requires[i]
			if r.blocked_edges[static_dependency_edge(pkgname, dependency)] {
				continue
			}
			r.collect(dependency) or {
				if r.options.debug {
					eprintln('cannot resolve ${dependency}: ${err.msg()}')
				}
				return error('could not resolve dependency ${dependency}: ${err.msg()}')
			}
		}
	}
	r.order.prepend(pkgname)
}

fn (mut r StaticResolver) package_order(pkgnames []string) ![]string {
	r.solve_state = map[string]u8{}
	r.blocked_edges = map[string]bool{}
	for pkgname in pkgnames {
		r.prepare(pkgname)!
	}
	r.visited = map[string]bool{}
	r.order = []string{}
	for i := pkgnames.len - 1; i >= 0; i-- {
		r.collect(pkgnames[i])!
	}
	return r.order.clone()
}

struct StaticFragmentMerger {
mut:
	fragments             []string
	merge_visible         []bool
	group_depth           int
	whole_archive_active  bool
	pending_operand_owner string
}

fn static_fragment_is_group_start(fragment string) bool {
	return fragment in ['-Wl,--start-group', '-Wl,-(']
}

fn static_fragment_is_group_end(fragment string) bool {
	return fragment in ['-Wl,--end-group', '-Wl,-)']
}

fn static_fragment_is_whole_archive_control(fragment string) bool {
	return fragment in ['-Wl,--whole-archive', '-Wl,--no-whole-archive']
}

fn static_fragment_is_search_path(fragment string) bool {
	return fragment.starts_with('-I') || fragment.starts_with('-L') || fragment.starts_with('-F')
}

fn static_fragment_takes_operand(fragment string) bool {
	return fragment in ['-L', '-l', '-framework', '-weak_framework', '-force_load', '-Xlinker',
		'-isystem', '-idirafter', '-include', '-Wl,-rpath', '-Wl,--rpath', '-Wl,-R',
		'-Wl,-rpath-link', '-Wl,--rpath-link', '-Wl,--version-script']
}

fn (m &StaticFragmentMerger) find_visible(fragment string) int {
	for i := m.fragments.len - 1; i >= 0; i-- {
		if m.merge_visible[i] && m.fragments[i] == fragment {
			return i
		}
	}
	return -1
}

fn (m &StaticFragmentMerger) should_merge_visible(previous int, fragment string) bool {
	if previous <= 0 || fragment.len <= 2 || !fragment.starts_with('-l') {
		return true
	}
	// pkgconf preserves a library when its immediate predecessor is a special linker fragment.
	return !m.fragments[previous - 1].starts_with('-Wl,')
}

fn (mut m StaticFragmentMerger) append_fragment(fragment string, is_private bool) {
	in_group := m.group_depth > 0
	in_whole_archive := m.whole_archive_active
	is_group_start := static_fragment_is_group_start(fragment)
	is_group_end := static_fragment_is_group_end(fragment)
	operand_owner := m.pending_operand_owner
	is_operand := operand_owner != ''
	m.pending_operand_owner = ''
	is_whole_archive_start := fragment == '-Wl,--whole-archive'
		|| (operand_owner == '-Xlinker' && fragment == '--whole-archive')
	is_whole_archive_end := fragment == '-Wl,--no-whole-archive'
		|| (operand_owner == '-Xlinker' && fragment == '--no-whole-archive')
	if is_group_start {
		m.group_depth++
	}
	if is_group_end && m.group_depth > 0 {
		m.group_depth--
	}
	if is_whole_archive_start {
		m.whole_archive_active = true
	}
	if is_whole_archive_end {
		m.whole_archive_active = false
	}
	is_positional := is_operand || in_group || is_group_start || is_group_end
		|| static_fragment_is_whole_archive_control(fragment) || in_whole_archive
		|| static_fragment_takes_operand(fragment)
	if is_private || is_positional {
		m.fragments << fragment
		m.merge_visible << !is_positional
	} else if static_fragment_is_search_path(fragment) {
		if m.find_visible(fragment) == -1 {
			m.fragments << fragment
			m.merge_visible << true
		}
	} else {
		previous := m.find_visible(fragment)
		if previous >= 0 && m.should_merge_visible(previous, fragment) {
			m.fragments.delete(previous)
			m.merge_visible.delete(previous)
		}
		m.fragments << fragment
		m.merge_visible << true
	}
	if static_fragment_takes_operand(fragment) {
		m.pending_operand_owner = fragment
	}
}

fn (mut m StaticFragmentMerger) append(fragments []string, is_private bool) {
	for fragment in fragments {
		m.append_fragment(fragment, is_private)
	}
}

fn resolve_dynamic(pkgnames []string, options Options) !ResolvedFlags {
	mut combined := &PkgConfig(unsafe { nil })
	for pkgname in pkgnames {
		mut pc := load(pkgname, options)!
		if unsafe { combined != 0 } {
			combined.extend(pc)
		} else {
			combined = pc
		}
	}
	return ResolvedFlags{
		cflags: combined.cflags.clone()
		libs:   combined.libs.clone()
	}
}

// resolve returns the pkgconf-compatible static fragment closure. Dynamic
// callers retain the legacy load/extend behavior.
pub fn resolve(pkgnames []string, options Options) !ResolvedFlags {
	if pkgnames.len == 0 {
		return error('No packages given')
	}
	if options.link_mode != .static_ {
		return resolve_dynamic(pkgnames, options)
	}
	mut resolver := StaticResolver{
		options: options
	}
	order := resolver.package_order(pkgnames)!
	mut cflags := StaticFragmentMerger{}
	mut libs := StaticFragmentMerger{}
	for pkgname in order {
		pc := resolver.packages[pkgname] or { return error('could not load package ${pkgname}') }
		cflags.append(pc.cflags, false)
	}
	for pkgname in order {
		pc := resolver.packages[pkgname] or { return error('could not load package ${pkgname}') }
		cflags.append(pc.cflags_private, true)
	}
	for pkgname in order {
		pc := resolver.packages[pkgname] or { return error('could not load package ${pkgname}') }
		libs.append(pc.libs, false)
		libs.append(pc.libs_private, true)
	}
	return ResolvedFlags{
		cflags: cflags.fragments
		libs:   libs.fragments
	}
}

fn (mut pc PkgConfig) add_path(path string) {
	if path == '' {
		return
	}
	p := path.trim_right('/')
	if !os.exists(p) {
		return
	}
	$if trace_pkgconfig_add_path ? {
		eprintln('> PkgConfig.add_path path: ${p}')
	}
	if pc.paths.index(p) == -1 {
		pc.paths << p
	}
}

fn (mut pc PkgConfig) load_paths() {
	// Allow for full custom user control over the default paths too, through
	// setting `PKG_CONFIG_PATH_DEFAULTS` to a list of search paths, separated
	// by `:`.
	split_c := $if windows { ';' } $else { ':' }
	config_path_override := os.getenv('PKG_CONFIG_PATH_DEFAULTS')
	if config_path_override != '' {
		for path in config_path_override.split(split_c) {
			pc.add_path(path)
		}
	} else {
		if pc.options.use_default_paths {
			for path in default_paths {
				pc.add_path(path)
			}
		}
	}
	for path in pc.options.path.split(split_c) {
		pc.add_path(path)
	}
	env_var := os.getenv('PKG_CONFIG_PATH')
	if env_var != '' {
		env_paths := env_var.trim_space().split(split_c)
		for path in env_paths {
			pc.add_path(path)
		}
	}
}

fn load_direct(pkgname string, options Options) !&PkgConfig {
	mut pc := &PkgConfig{
		modname: pkgname
		options: options
	}
	pc.load_paths()
	file := pc.resolve(pkgname) or { return err }
	pc.parse(file) or { return error('file "${file}" could not be parsed: ${err.msg()}') }
	return pc
}

pub fn load(pkgname string, options Options) !&PkgConfig {
	mut pc := &PkgConfig{
		modname: pkgname
		options: options
	}
	pc.load_paths()
	file := pc.resolve(pkgname) or { return err }
	pc.parse(file) or { return error('file "${file}" could not be parsed: ${err.msg()}') }
	if !options.norecurse {
		pc.load_requires()!
	}
	return pc
}

pub fn list() []string {
	mut pc := &PkgConfig{
		options: Options{}
	}
	pc.load_paths()
	mut modules := []string{}
	for path in pc.paths {
		files := os.ls(path) or { continue }
		for file in files {
			if file.ends_with('.pc') {
				name := file.replace('.pc', '')
				if modules.index(name) == -1 {
					modules << name
				}
			}
		}
	}
	return modules
}
