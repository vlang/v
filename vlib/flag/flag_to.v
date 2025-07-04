module flag

struct FlagData {
	raw        string @[required]
	field_name string @[required]
	delimiter  string
	name       string
	arg        ?string
	pos        int
	repeats    int = 1
}

struct FlagContext {
	raw       string @[required] // raw arg array entry. E.g.: `--id=val`
	delimiter string // usually `'-'`
	name      string // either struct field name or what is defined in `@[long: <name>]`
	next      string // peek at what is the next flag/arg
	pos       int    // position in arg array
}

pub enum ParseMode {
	strict  // return errors for unknown or malformed flags per default
	relaxed // relax flag match errors and add them to `no_match` list instead
}

pub enum Style {
	short         // Posix short only, allows multiple shorts -def is `-d -e -f` and "sticky" arguments e.g.: `-ofoo` = `-o foo`
	long          // GNU style long option *only*. E.g.: `--name` or `--name=value`
	short_long    // extends `posix` style shorts with GNU style long options: `--flag` or `--name=value`
	v             // V style flags as found in flags for the `v` compiler. Single flag denote `-` followed by string identifier e.g.: `-verbose`, `-name value`, `-v`, `-n value` or `-d ident=value`
	v_flag_parser // V `flag.FlagParser` style flags as supported by `flag.FlagParser`. Long flag denote `--` followed by string identifier e.g.: `--verbose`, `--name value`, `-v` or `-n value`.
	go_flag       // GO `flag` module style. Single flag denote `-` followed by string identifier e.g.: `-verbose`, `-name value`, `-v` or `-n value` and both long `--name value` and GNU long `--name=value`
	cmd_exe       // `cmd.exe` style flags. Single flag denote `/` followed by lower- or upper-case character
}

struct StructInfo {
	name   string            // name of the struct itself
	attrs  map[string]string // collection of `@[x: y]` sat on the struct, read via reflection
	fields map[string]StructField
}

@[flag]
pub enum FieldHints {
	is_bool
	is_array
	is_ignore
	is_int_type
	has_tail
	short_only
	can_repeat
}

// StructField is a representation of the data collected via reflection on the input `T` struct.
// `match_name` is resolved upon reflection and represents the longest flag name value the user wants
// to be mapped to this field. If users has indicated that they want to match a field as a short/abbrevation flag
// `short` is accounted instead of `match_name`
struct StructField {
	name       string // name of the field on the struct, *not used* for resolving mappings, use `match_name`
	match_name string // match_name is either `name` or the value of `@[long: x]` or `@[only: x]`
	short      string // single char short alias of field, sat via `@[short: x]` or `@[only: x]`
	hints      FieldHints = FieldHints.zero()
	attrs      map[string]string // collection of `@[x: y]` sat on the field, read via reflection
	type_name  string
	doc        string // documentation string sat via `@[xdoc: x y z]`
}

fn (sf StructField) shortest_match_name() ?string {
	mut name := sf.short // if `short` is sat it takes precedence over `match_name`
	if name == '' && sf.match_name.len == 1 {
		name = sf.match_name
	}
	if name != '' {
		return name
	}
	return none
}

@[params]
pub struct ParseConfig {
pub:
	delimiter string    = '-'         // delimiter used for flags
	mode      ParseMode = .strict     // return errors for unknown or malformed flags per default
	style     Style     = .short_long // expected flag style
	stop      ?string // single, usually '--', string that stops parsing flags/options
	skip      u16     // skip this amount in the input argument array, usually `1` for skipping executable or subcmd entry
}

@[params]
pub struct DocConfig {
pub:
	delimiter string = '-'         // delimiter used for flags
	style     Style  = .short_long // expected flag style
pub mut:
	name        string            // application name
	version     string            // application version
	description string            // application description
	footer      string            // application description footer written after auto-generated flags list/ field descriptions
	layout      DocLayout         // documentation layout
	options     DocOptions        // documentation options
	fields      map[string]string // doc strings for each field (overwrites @[doc: xxx] attributes)
}

@[flag]
pub enum Show {
	name
	version
	flags
	flag_type
	flag_hint
	description
	flags_header
	footer
}

pub struct DocLayout {
pub mut:
	description_padding int = 28
	description_width   int = 50
	flag_indent         int = 2
}

pub struct DocOptions {
pub mut:
	flag_header string = '\nOptions:'
	compact     bool
	show        Show = ~Show.zero()
}

// max_width returns the total width of the `DocLayout`.
pub fn (dl DocLayout) max_width() int {
	return dl.flag_indent + dl.description_padding + dl.description_width
}

pub struct FlagMapper {
pub:
	config ParseConfig @[required]
	input  []string    @[required]
mut:
	si                   StructInfo
	handled_pos          []int // tracks handled positions in the `input` args array. NOTE: can contain duplicates
	field_map_flag       map[string]FlagData
	array_field_map_flag map[string][]FlagData
	no_match             []int // indicies of unmatched flags in the `input` array
}

@[if trace_flag_mapper ?]
fn trace_println(str string) {
	println(str)
}

@[if trace_flag_mapper ? && debug]
fn trace_dbg_println(str string) {
	println(str)
}

// dbg_match returns a debug string with data about the mapping of a flag to a field
fn (fm FlagMapper) dbg_match(flag_ctx FlagContext, field StructField, arg string, field_extra string) string {
	struct_name := fm.si.name
	extra := if field_extra != '' { '/' + field_extra } else { '' }
	return '${struct_name}.${field.name}/${field.short}${extra} in ${flag_ctx.raw}/${flag_ctx.name} = `${arg}`'
}

fn (fm FlagMapper) get_struct_info[T]() !StructInfo {
	mut struct_fields := map[string]StructField{}
	mut struct_attrs := map[string]string{}
	mut struct_name := ''
	mut used_names := []string{}
	$if T is $struct {
		struct_name = T.name
		trace_println('${@FN}: mapping struct "${struct_name}"...')

		$for st_attr in T.attributes {
			if st_attr.has_arg && st_attr.kind == .string {
				struct_attrs[st_attr.name.trim_space()] = st_attr.arg.trim(' ')
			}
		}
		// Handle positional first so they can be marked as handled
		$for field in T.fields {
			mut hints := FieldHints.zero()
			mut match_name := field.name.replace('_', '-')
			trace_println('${@FN}: field "${field.name}":')
			mut attrs := map[string]string{}
			for attr in field.attrs {
				trace_println('\tattribute: "${attr}"')
				if attr.contains(':') {
					split := attr.split(':')
					attrs[split[0].trim_space()] = split[1].trim(' ')
				} else {
					attrs[attr.trim(' ')] = 'true'
				}
			}
			if long_alias := attrs['long'] {
				match_name = long_alias.replace('_', '-')
			}
			if only := attrs['only'] {
				if only.len == 0 {
					return error('attribute @[only] on ${struct_name}.${match_name} can not be empty, use @[only: x]')
				} else if only.len == 1 {
					hints.set(.short_only)
					attrs['short'] = only
					if only in used_names {
						return error('attribute @[only: ${only}] on ${struct_name}.${field.name}, "${only}" is already in use')
					}
				} else if only.len > 1 {
					match_name = only
				}
			}

			mut short := ''
			if short_alias := attrs['short'] {
				if short_alias.len != 1 {
					return error('attribute @[short: ${short}] on ${struct_name}.${field.name} can only be a single character')
				}
				short = short_alias

				if short in used_names {
					return error('attribute @[short: ${short_alias}] on ${struct_name}.${field.name}, "${short}" is already in use')
				}
				used_names << short
			}

			trace_println('\tmatch name: "${match_name}"')
			used_names << match_name

			$if field.typ is int {
				hints.set(.is_int_type)
			} $else $if field.typ is i64 {
				hints.set(.is_int_type)
			} $else $if field.typ is u64 {
				hints.set(.is_int_type)
			} $else $if field.typ is i32 {
				hints.set(.is_int_type)
			} $else $if field.typ is u32 {
				hints.set(.is_int_type)
			} $else $if field.typ is i16 {
				hints.set(.is_int_type)
			} $else $if field.typ is u16 {
				hints.set(.is_int_type)
			} $else $if field.typ is i8 {
				hints.set(.is_int_type)
			} $else $if field.typ is u8 {
				hints.set(.is_int_type)
			}

			if _ := attrs['repeats'] {
				hints.set(.can_repeat)
			}
			if _ := attrs['tail'] {
				hints.set(.has_tail)
			}
			if _ := attrs['ignore'] {
				hints.set(.is_ignore)
			}

			$if field.typ is bool {
				trace_println('\tfield "${field.name}" is a bool')
				hints.set(.is_bool)
			}
			$if field.is_array {
				trace_println('\tfield "${field.name}" is array')
				hints.set(.is_array)
			}

			if !hints.has(.is_int_type) && hints.has(.can_repeat) {
				return error('`@[repeats] can only be used on integer type fields like `int`, `u32` etc.')
			}

			mut doc := ''
			// `xdoc` was chosen because of `vfmt` sorting attributes alphabetically - so to avoid cluttering attributes
			// we want the doc strings to be among the last attributes. To make it flexible to users we provide
			// `$d('v:flag:doc_attr','xdoc')` to let users tweak it.
			if docs := attrs[$d('v:flag:doc_attr', 'xdoc')] {
				trace_println('\tdoc for field "${field.name}": ${docs}')
				doc = docs
			}

			struct_fields[field.name] = StructField{
				name:       field.name
				match_name: match_name
				hints:      hints
				short:      short
				attrs:      attrs
				type_name:  typeof(field).name
				doc:        doc
			}
		}
	} $else {
		return error('the type `${T.name}` can not be decoded.')
	}

	return StructInfo{
		name:   struct_name
		attrs:  struct_attrs
		fields: struct_fields
	}
}

fn (m map[string]FlagData) query_flag_with_name(name string) ?FlagData {
	for _, flag_data in m {
		if flag_data.name == name {
			return flag_data
		}
	}
	return none
}

// to_struct returns `T` with field values sat to any matching flags in `input`.
// to_struct also returns any flags from `input`, in order of appearance, that could *not* be matched
// with any field on `T`.
pub fn to_struct[T](input []string, config ParseConfig) !(T, []string) {
	mut fm := FlagMapper{
		config: config
		input:  input
	}
	fm.parse[T]()!
	st := fm.to_struct[T](none)!
	return st, fm.no_matches()
}

// using returns `defaults` with field values overwritten with any matching flags in `input`.
// Any field that could *not* be matched with a flag will have the same value as the
// field(s) passed as `defaults`.
// using also returns any flags from `input`, in order of appearance, that could *not* be matched
// with any field on `T`.
pub fn using[T](defaults T, input []string, config ParseConfig) !(T, []string) {
	mut fm := FlagMapper{
		config: config
		input:  input
	}
	fm.parse[T]()!
	st := fm.to_struct[T](defaults)!
	return st, fm.no_matches()
}

// to_doc returns a "usage" style documentation `string` generated from attributes on `T` or via the `dc` argument.
pub fn to_doc[T](dc DocConfig) !string {
	mut fm := FlagMapper{
		config: ParseConfig{
			delimiter: dc.delimiter
			style:     dc.style
		}
		input:  []
	}
	fm.si = fm.get_struct_info[T]()!
	return fm.to_doc(dc)!
}

// no_matches returns any flags from the `input` array, in order of appearance, that could *not* be matched against any fields.
// This method should be called *after* `to_struct[T]()`.
pub fn (fm FlagMapper) no_matches() []string {
	mut non_matching := []string{}
	for i in fm.no_match {
		non_matching << fm.input[i]
	}
	return non_matching
}

// parse parses `T` to an internal data representation.
pub fn (mut fm FlagMapper) parse[T]() ! {
	config := fm.config
	style := config.style
	delimiter := config.delimiter
	args := fm.input

	trace_println('${@FN}: parsing ${args}')
	if config.skip > 0 {
		// skip X entries. Could be the "exe", "executable" or "subcmd" - or more if the user wishes
		for i in 0 .. int(config.skip) {
			fm.handled_pos << i
		}
	}

	// Gather runtime information about T
	fm.si = fm.get_struct_info[T]()!
	struct_name := fm.si.name

	// Find the position of the last flag in `input`
	mut pos_last_flag := -1
	for pos, arg in args {
		if arg.starts_with(delimiter) {
			pos_last_flag = pos
		}
	}

	for pos, arg in args {
		if arg == '' {
			fm.no_match << pos
			continue
		}
		mut pos_is_handled := pos in fm.handled_pos

		if !pos_is_handled {
			// Stop parsing as soon as possible if `--` (or user defined) stop option is sat and encountered
			if arg == config.stop or { '' } {
				trace_println('${@FN}: reached option stop (${config.stop}) at index ${pos}')
				// record all positions after this as not matching, unless pos is the last entry
				if pos < args.len - 1 {
					for unused_pos in pos + 1 .. args.len {
						fm.no_match << unused_pos
					}
				}
				break
			}
		}

		// peek next arg
		mut next := if pos + 1 < args.len { args[pos + 1] } else { '' }

		// Parse arg entry (potential flag)
		mut is_flag := false // `flag` starts with `-` (default value) or user defined
		mut flag_name := ''
		if arg.starts_with(delimiter) {
			is_flag = true
			flag_name = arg.trim_left(delimiter)
			// Parse GNU (GO `flag`) `--name=value`
			if style in [.long, .short_long, .go_flag] {
				flag_name = flag_name.all_before('=')
			}
		}

		// A flag, find best matching field in struct, if any
		if is_flag {
			// Figure out and validate used delimiter
			used_delimiter := arg.all_before(flag_name)
			is_long_delimiter := used_delimiter.count(delimiter) == 2
			is_short_delimiter := used_delimiter.count(delimiter) == 1
			is_invalid_delimiter := !is_long_delimiter && !is_short_delimiter
			if is_invalid_delimiter {
				if config.mode == .relaxed {
					fm.no_match << pos
					continue
				}
				return error('invalid delimiter `${used_delimiter}` for flag `${arg}`')
			}
			if is_long_delimiter {
				if style == .v {
					if config.mode == .relaxed {
						fm.no_match << pos
						continue
					}
					return error('long delimiter `${used_delimiter}` encountered in flag `${arg}` in ${style} (V) style parsing mode. Maybe you meant `.v_flag_parser`?')
				}
				if style == .short {
					if config.mode == .relaxed {
						fm.no_match << pos
						continue
					}
					return error('long delimiter `${used_delimiter}` encountered in flag `${arg}` in ${style} (POSIX) style parsing mode')
				}
			}

			if is_short_delimiter {
				if style == .long {
					if config.mode == .relaxed {
						fm.no_match << pos
						continue
					}
					return error('short delimiter `${used_delimiter}` encountered in flag `${arg}` in ${style} (GNU) style parsing mode')
				}
				if style == .short_long && flag_name.len > 1 && flag_name.contains('-') {
					if config.mode == .relaxed {
						fm.no_match << pos
						continue
					}
					return error('long name `${flag_name}` used with short delimiter `${used_delimiter}` in flag `${arg}` in ${style} (POSIX/GNU) style parsing mode')
				}
			}

			if flag_name == '' {
				if config.mode == .relaxed {
					fm.no_match << pos
					continue
				}
				return error('invalid delimiter-only flag `${arg}`')
			}

			flag_ctx := FlagContext{
				raw:       arg
				delimiter: used_delimiter
				name:      flag_name
				next:      next
				pos:       pos
			}

			// Identify and match short clusters first. Example: `-yxz( arg)` = `-y -x -z( arg)`
			if is_short_delimiter && style in [.short, .short_long] {
				fm.map_posix_short_cluster(flag_ctx)!
			}

			pos_is_handled = pos in fm.handled_pos
			if pos_is_handled {
				trace_dbg_println('${@FN}: skipping position "${pos}". Already handled')
				continue
			}

			for _, field in fm.si.fields {
				if field.hints.has(.is_ignore) {
					trace_dbg_println('${@FN}: skipping field "${field.name}" has an @[ignore] attribute')
					continue
				}
				// Field already identified, skip
				if _ := fm.field_map_flag[field.name] {
					trace_dbg_println('${@FN}: skipping field "${field.name}" already identified')
					continue
				}

				trace_println('${@FN}: matching `${used_delimiter}` ${if is_long_delimiter {
					'(long)'
				} else {
					'(short)'
				}} flag "${arg}/${flag_name}" is it matching "${field.name}${if field.short != '' {
					'/' + field.short
				} else {
					''
				}}"?')

				if field.hints.has(.short_only) {
					trace_println('${@FN}: skipping long delimiter `${used_delimiter}` match for ${struct_name}.${field.name} since it has [only: ${field.short}]')
				}

				if is_short_delimiter {
					if style in [.short, .short_long] {
						if fm.map_posix_short(flag_ctx, field)! {
							continue
						}
					} else if style == .v {
						if fm.map_v(flag_ctx, field)! {
							continue
						}
					} else if style == .v_flag_parser {
						if fm.map_v_flag_parser_short(flag_ctx, field)! {
							continue
						}
					} else if style == .cmd_exe {
						if fm.map_cmd_exe(flag_ctx, field)! {
							continue
						}
					} else if style == .go_flag {
						if fm.map_go_flag_short(flag_ctx, field)! {
							continue
						}
					}
				}

				if is_long_delimiter {
					// Parse GNU `--name=value`
					if style in [.long, .short_long] {
						if fm.map_gnu_long(flag_ctx, field)! {
							continue
						}
					} else if style == .v_flag_parser {
						if fm.map_v_flag_parser_long(flag_ctx, field)! {
							continue
						}
					} else if style == .go_flag {
						if fm.map_go_flag_long(flag_ctx, field)! {
							continue
						}
					}
				}
			}
		}

		// Extract any tail(s) according to config
		if pos >= pos_last_flag + 1 {
			trace_dbg_println('${@FN}: (tail) looking for tail match for position "${pos}"...')
			pos_is_handled = pos in fm.handled_pos
			if pos_is_handled {
				trace_dbg_println('${@FN}: (tail) skipping position "${pos}". Already handled')
				continue
			}
			for _, field in fm.si.fields {
				if field.hints.has(.is_ignore) {
					trace_dbg_println('${@FN}: (tail) skipping field "${field.name}" has an @[ignore] attribute')
					continue
				}
				// Field already mapped, skip
				if _ := fm.field_map_flag[field.name] {
					trace_dbg_println('${@FN}: (tail) skipping field "${field.name}" already identified')
					continue
				}
				if field.hints.has(.has_tail) {
					trace_dbg_println('${@FN}: (tail) field "${field.name}" has a tail attribute. fm.handled_pos.len: ${fm.handled_pos.len}')
					last_handled_pos := fm.handled_pos[fm.handled_pos.len - 1] or { 0 }
					trace_println('${@FN}: (tail) flag `${arg}` last_handled_pos: ${last_handled_pos} pos: ${pos}')
					if pos == last_handled_pos + 1 || pos == pos_last_flag + 1 {
						if field.hints.has(.is_array) {
							fm.array_field_map_flag[field.name] << FlagData{
								raw:        arg
								field_name: field.name
								arg:        ?string(arg) // .arg is used when assigning at comptime to []XYZ
								pos:        pos
							}
						} else {
							fm.field_map_flag[field.name] = FlagData{
								raw:        arg
								field_name: field.name
								arg:        ?string(arg)
								pos:        pos
							}
						}
						fm.handled_pos << pos
						continue
					}
				}
			}
		}
		if pos !in fm.handled_pos && pos !in fm.no_match {
			fm.no_match << pos
			// TODO: flag_name := arg.trim_left(delimiter) // WHY?
			if already_flag := fm.field_map_flag.query_flag_with_name(arg.trim_left(delimiter)) {
				return error('flag `${arg} ${next}` is already mapped to field `${already_flag.field_name}` via `${already_flag.delimiter}${already_flag.name} ${already_flag.arg or {
					''
				}}`')
			}
		}
	}
}

// to_doc returns a "usage" style documentation `string` generated from the internal data structures generated via the `parse()` function.
pub fn (fm FlagMapper) to_doc(dc DocConfig) !string {
	mut docs := []string{}

	mut name_and_version := ''
	// resolve name
	if dc.options.show.has(.name) {
		mut app_name := ''
		// struct `name: x` attribute, if defined
		if attr_name := fm.si.attrs['name'] {
			app_name = attr_name
		}
		// user passed `name` overrides the attribute and default name
		if dc.name != '' {
			app_name = dc.name
		}
		if app_name != '' {
			name_and_version = '${app_name}'
		}
	}
	// resolve version
	if dc.options.show.has(.version) {
		mut app_version := ''
		// struct `version` attribute, if defined
		if attr_version := fm.si.attrs['version'] {
			app_version = attr_version
		}
		// user passed `version` overrides the attribute
		if dc.version != '' {
			app_version = dc.version
		}

		if app_version != '' {
			if name_and_version != '' {
				name_and_version = '${name_and_version} ${app_version}'
			} else {
				name_and_version = '${app_version}'
			}
		}
	}

	if name_and_version != '' {
		docs << '${name_and_version}'
	}

	// Resolve the description if visible
	if dc.options.show.has(.description) {
		mut description := ''
		// Set the description from any `xdoc` (or user defined) from *struct*
		if attr_desc := fm.si.attrs[$d('v:flag:doc_attr', 'xdoc')] {
			description = attr_desc
		}
		// user passed description overrides the attribute
		if dc.description != '' {
			description = dc.description
		}
		if description != '' {
			docs << keep_at_max(description, dc.layout.max_width())
		}
	}

	if dc.options.show.has(.flags) {
		fields_docs := fm.fields_docs(dc)!
		if fields_docs.len > 0 {
			if dc.options.show.has(.flags_header) {
				docs << dc.options.flag_header
			}
			docs << fields_docs
		}
	}

	if dc.options.show.has(.footer) {
		mut footer := ''
		// struct `footer` attribute, if defined
		if attr_footer := fm.si.attrs['footer'] {
			footer = attr_footer
		}
		// user passed `footer` overrides the attribute
		if dc.footer != '' {
			footer = dc.footer
		}
		if footer != '' {
			docs << keep_at_max(footer, dc.layout.max_width())
		}
	}

	if name_and_version != '' {
		mut longest_line := 0
		for doc_line in docs {
			lines := doc_line.split('\n')
			for line in lines {
				if line.len > longest_line {
					longest_line = line.len
				}
			}
		}
		docs.insert(1, '-'.repeat(longest_line))
	}
	return docs.join('\n')
}

// fields_docs returns every line of the combined field documentation.
pub fn (fm FlagMapper) fields_docs(dc DocConfig) ![]string {
	short_delimiter := match dc.style {
		.short, .short_long, .v, .v_flag_parser, .go_flag, .cmd_exe { dc.delimiter }
		.long { dc.delimiter.repeat(2) }
	}
	long_delimiter := match dc.style {
		.short, .v, .go_flag, .cmd_exe { dc.delimiter }
		.long, .v_flag_parser, .short_long { dc.delimiter.repeat(2) }
	}

	pad_desc := if dc.layout.description_padding < 0 { 0 } else { dc.layout.description_padding }
	empty_padding := ' '.repeat(pad_desc)
	indent_flags := if dc.layout.flag_indent < 0 { 0 } else { dc.layout.flag_indent }
	indent_flags_padding := ' '.repeat(indent_flags)
	desc_max := if dc.layout.description_width < 1 { 1 } else { dc.layout.description_width }

	mut docs := []string{}
	for _, field in fm.si.fields {
		if field.hints.has(.is_ignore) {
			trace_println('${@FN}: skipping field "${field.name}" has an @[ignore] attribute')
			continue
		}
		doc := dc.fields[field.name] or { field.doc }
		short := field.shortest_match_name() or { '' }
		long := field.match_name

		// -f, --flag <type>
		mut flag_line := indent_flags_padding
		flag_line += if short != '' { '${short_delimiter}${short}' } else { '' }
		if !field.hints.has(.short_only) && long != '' {
			if short != '' {
				flag_line += ', '
			}
			flag_line += '${long_delimiter}${long}'
		}
		if dc.options.show.has(.flag_type) && field.type_name != 'bool' {
			if !field.hints.has(.can_repeat) {
				flag_line += ' <${field.type_name.replace('[]', '')}>'
			}
		}
		if dc.options.show.has(.flag_hint) {
			if field.hints.has(.is_array) {
				flag_line += ' (allowed multiple times)'
			}
			if field.hints.has(.can_repeat) {
				flag_line += ', ${short_delimiter}${short}${short}${short}... (can repeat)'
			}
		}
		flag_line_diff := flag_line.len - pad_desc
		if flag_line_diff < 0 {
			// This makes sure the description is put on a new line if the flag line is
			// longer than the padding.
			diff := -flag_line_diff
			line := flag_line + ' '.repeat(diff) +
				keep_at_max(doc, desc_max).replace('\n', '\n${empty_padding}')
			docs << line.trim_space_right()
		} else {
			docs << flag_line.trim_space_right()
			if doc != '' {
				line := empty_padding +
					keep_at_max(doc, desc_max).replace('\n', '\n${empty_padding}')
				docs << line.trim_space_right()
			}
		}
		if !dc.options.compact {
			docs << ''
		}
	}
	// Look for custom flag entries starting with delimiter
	for entry, doc in dc.fields {
		if entry.starts_with(dc.delimiter) {
			flag_line_diff := entry.len - pad_desc + indent_flags
			if flag_line_diff < 0 {
				// This makes sure the description is put on a new line if the flag line is
				// longer than the padding.
				diff := -flag_line_diff
				line := indent_flags_padding + entry.trim(' ') + ' '.repeat(diff) +
					keep_at_max(doc, desc_max).replace('\n', '\n${empty_padding}')
				docs << line.trim_space_right()
			} else {
				docs << indent_flags_padding + entry.trim(' ')
				line := empty_padding +
					keep_at_max(doc, desc_max).replace('\n', '\n${empty_padding}')
				docs << line.trim_space_right()
			}
			if !dc.options.compact {
				docs << ''
			}
		}
	}
	if docs.len > 0 {
		if !dc.options.compact {
			// In non-compact mode the last item will be an empty line, delete it
			docs.delete_last()
		}
	}
	return docs
}

// keep_at_max returns `str` that is kept at a line width of `max` characters.
// User provided newlines is ignored in case the `str` has to be corrected to
// fit the provided `max` width value. Lines longer than `max` are not dealt with.
fn keep_at_max(str string, max int) string {
	safe_max := if max <= 0 { 1 } else { max }
	if str.len <= safe_max || str.count(' ') == 0 {
		return str
	}
	mut fitted := ''
	mut width := 0
	mut last_possible_break := str.index(' ') or { 0 }
	mut never_touched := true
	s := str.trim_space()
	for i, c in s {
		width++
		if c == ` ` {
			last_possible_break = i
		} else if c == `\n` {
			width = 0
		}
		if width == safe_max {
			never_touched = false
			fitted = s[..last_possible_break] + '\n'
			// At this point we are refitting the doc string so user provided newlines can not be kept
			// ... at least not without a tremendous increase in code complexity, that highly likely
			// will not suit all use-cases anyway.
			fitted += keep_at_max(s[last_possible_break..].replace('\n', ' ').trim(' '),
				safe_max).trim(' ')
		} else if width > safe_max {
			break
		}
	}
	if never_touched {
		return str
	}
	return fitted
}

// to_struct returns `defaults` or a new instance of `T` that has the parsed flags from `input` mapped to the fields of struct `T`.
pub fn (fm FlagMapper) to_struct[T](defaults ?T) !T {
	// Generate T result
	mut result := defaults or { T{} }
	the_default := defaults or { T{} }

	$if T is $struct {
		struct_name := T.name
		$for field in T.fields {
			if f := fm.field_map_flag[field.name] {
				a_or_r := f.arg or { '${f.repeats}' }
				$if field.typ is int {
					// TODO: find a way to move this kind of duplicate code out
					if !a_or_r.is_int() {
						return error('can not assign non-integer value `${a_or_r}` from flag `${f.raw}` to `${struct_name}.${field.name}`')
					}
					trace_dbg_println('${@FN}: assigning (int) ${struct_name}.${field.name} = ${a_or_r}')
					result.$(field.name) = a_or_r.int()
				} $else $if field.typ is i64 {
					trace_dbg_println('${@FN}: assigning (i64) ${struct_name}.${field.name} = ${a_or_r}')
					if !a_or_r.is_int() {
						return error('can not assign non-integer value `${a_or_r}` from flag `${f.raw}` to `${struct_name}.${field.name}`')
					}
					result.$(field.name) = a_or_r.i64()
				} $else $if field.typ is u64 {
					trace_dbg_println('${@FN}: assigning (u64) ${struct_name}.${field.name} = ${a_or_r}')
					if !a_or_r.is_int() {
						return error('can not assign non-integer value `${a_or_r}` from flag `${f.raw}` to `${struct_name}.${field.name}`')
					}
					result.$(field.name) = a_or_r.u64()
				} $else $if field.typ is i32 {
					trace_dbg_println('${@FN}: assigning (i32) ${struct_name}.${field.name} = ${a_or_r}')
					if !a_or_r.is_int() {
						return error('can not assign non-integer value `${a_or_r}` from flag `${f.raw}` to `${struct_name}.${field.name}`')
					}
					result.$(field.name) = a_or_r.i32()
				} $else $if field.typ is u32 {
					trace_dbg_println('${@FN}: assigning (u32) ${struct_name}.${field.name} = ${a_or_r}')
					if !a_or_r.is_int() {
						return error('can not assign non-integer value `${a_or_r}` from flag `${f.raw}` to `${struct_name}.${field.name}`')
					}
					result.$(field.name) = a_or_r.u32()
				} $else $if field.typ is i16 {
					trace_dbg_println('${@FN}: assigning (i16) ${struct_name}.${field.name} = ${a_or_r}')
					if !a_or_r.is_int() {
						return error('can not assign non-integer value `${a_or_r}` from flag `${f.raw}` to `${struct_name}.${field.name}`')
					}
					result.$(field.name) = a_or_r.i16()
				} $else $if field.typ is u16 {
					trace_dbg_println('${@FN}: assigning (u16) ${struct_name}.${field.name} = ${a_or_r}')
					if !a_or_r.is_int() {
						return error('can not assign non-integer value `${a_or_r}` from flag `${f.raw}` to `${struct_name}.${field.name}`')
					}
					result.$(field.name) = a_or_r.u16()
				} $else $if field.typ is i8 {
					trace_dbg_println('${@FN}: assigning (i8) ${struct_name}.${field.name} = ${a_or_r}')
					if !a_or_r.is_int() {
						return error('can not assign non-integer value `${a_or_r}` from flag `${f.raw}` to `${struct_name}.${field.name}`')
					}
					result.$(field.name) = a_or_r.i8()
				} $else $if field.typ is u8 {
					trace_dbg_println('${@FN}: assigning (u8) ${struct_name}.${field.name} = ${a_or_r}')
					if !a_or_r.is_int() {
						return error('can not assign non-integer value `${a_or_r}` from flag `${f.raw}` to `${struct_name}.${field.name}`')
					}
					result.$(field.name) = a_or_r.u8()
				} $else $if field.typ is f32 {
					result.$(field.name) = f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.f32()
				} $else $if field.typ is f64 {
					result.$(field.name) = f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.f64()
				} $else $if field.typ is bool {
					if arg := f.arg {
						return error('can not assign `${arg}` to bool field `${field.name}`')
					}
					result.$(field.name) = !the_default.$(field.name)
				} $else $if field.typ is string {
					trace_dbg_println('${@FN}: assigning (string) ${struct_name}.${field.name} = ${f.arg or {
						'ERROR'
					}
						.str()}')
					result.$(field.name) = f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.str()
				} $else {
					return error('field type: ${field.typ} for ${field.name} is not supported')
				}
			}
			for f in fm.array_field_map_flag[field.name] {
				// trace_println('${@FN}: appending ${field.name} << ${f.arg}')
				$if field.typ is []string {
					// TODO: find a way to move this kind of duplicate code out
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.str()
				} $else $if field.typ is []int {
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.int()
				} $else $if field.typ is []i64 {
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.i64()
				} $else $if field.typ is []u64 {
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.u64()
				} $else $if field.typ is []i32 {
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.i32()
				} $else $if field.typ is []u32 {
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.u32()
				} $else $if field.typ is []i16 {
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.i16()
				} $else $if field.typ is []u16 {
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.u16()
				} $else $if field.typ is []i8 {
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.i8()
				} $else $if field.typ is []u8 {
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.u8()
				} $else $if field.typ is []f32 {
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.f32()
				} $else $if field.typ is []f64 {
					result.$(field.name) << f.arg or {
						return error('failed appending ${f.raw} to ${field.name}')
					}
						.f64()
				} $else {
					return error('field type: ${field.typ} for multi value ${field.name} is not supported')
				}
			}
		}
	} $else {
		return error('the type `${T.name}` can not be decoded.')
	}

	return result
}

// map_v returns `true` if the V style flag in `flag_ctx` can be mapped to `field`.
// map_v adds data of the match in the internal structures for further processing if applicable
fn (mut fm FlagMapper) map_v(flag_ctx FlagContext, field StructField) !bool {
	flag_raw := flag_ctx.raw
	flag_name := flag_ctx.name
	pos := flag_ctx.pos
	used_delimiter := flag_ctx.delimiter
	next := flag_ctx.next

	if field.hints.has(.is_bool) {
		if flag_name == field.match_name {
			arg := if flag_raw.contains('=') { flag_raw.all_after('=') } else { '' }
			if arg != '' {
				return error('flag `${flag_raw}` can not be assigned to bool field "${field.name}"')
			}
			trace_println('${@FN}: found match for (bool) ${fm.dbg_match(flag_ctx, field,
				'true', '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				pos:        pos
			}
			fm.handled_pos << pos
			return true
		}
	}

	if flag_name == field.match_name || flag_name == field.short {
		if field.hints.has(.is_array) {
			trace_println('${@FN}: found match for (V style multiple occurrences) ${fm.dbg_match(flag_ctx,
				field, next, '')}')
			fm.array_field_map_flag[field.name] << FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(next)
				pos:        pos
			}
		} else {
			trace_println('${@FN}: found match for (V style) ${fm.dbg_match(flag_ctx,
				field, next, '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(next)
				pos:        pos
			}
		}
		fm.handled_pos << pos
		fm.handled_pos << pos + 1 // arg
		return true
	}
	return false
}

// map_v_flag_parser_short returns `true` if the V `flag.FlagParser` short (-) flag in `flag_ctx` can be mapped to `field`.
// map_v_flag_parser_short adds data of the match in the internal structures for further processing if applicable
fn (mut fm FlagMapper) map_v_flag_parser_short(flag_ctx FlagContext, field StructField) !bool {
	flag_raw := flag_ctx.raw
	flag_name := flag_ctx.name
	pos := flag_ctx.pos
	used_delimiter := flag_ctx.delimiter
	next := flag_ctx.next

	if flag_name.len != 1 {
		return error('`${flag_raw}` is not supported in V `flag.FlagParser` (short) style parsing mode. Only single character flag names are supported. Use `-f value` instead')
	}

	if flag_raw.contains('=') {
		return error('`=` in flag `${flag_raw}` is not supported in V `flag.FlagParser` (short) style parsing mode. Use `-f value` instead')
	}

	if field.hints.has(.is_bool) {
		if flag_name == field.match_name || flag_name == field.short {
			trace_println('${@FN}: found match for (bool) ${fm.dbg_match(flag_ctx, field,
				'true', '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				pos:        pos
			}
			fm.handled_pos << pos
			return true
		}
	}

	if flag_name == field.match_name || flag_name == field.short {
		if field.hints.has(.is_array) {
			trace_println('${@FN}: found match for V (`flag.FlagParser` (short) style multiple occurrences) ${fm.dbg_match(flag_ctx,
				field, next, '')}')
			fm.array_field_map_flag[field.name] << FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(next)
				pos:        pos
			}
		} else {
			trace_println('${@FN}: found match for V (`flag.FlagParser` (short) style) ${fm.dbg_match(flag_ctx,
				field, next, '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(next)
				pos:        pos
			}
		}
		fm.handled_pos << pos
		fm.handled_pos << pos + 1 // arg
		return true
	}
	return false
}

// map_v_flag_parser_long returns `true` if the V `flag.FlagParser` long (--) style flag in `flag_ctx` can be mapped to `field`.
// map_v_flag_parser_long adds data of the match in the internal structures for further processing if applicable
fn (mut fm FlagMapper) map_v_flag_parser_long(flag_ctx FlagContext, field StructField) !bool {
	flag_raw := flag_ctx.raw
	flag_name := flag_ctx.name
	pos := flag_ctx.pos
	used_delimiter := flag_ctx.delimiter
	next := flag_ctx.next

	if flag_raw.contains('=') {
		return error('`=` in flag `${flag_raw}` is not supported in V `flag.FlagParser` (long) style parsing mode. Use `--flag value` instead')
	}

	if field.hints.has(.is_bool) {
		if flag_name == field.match_name {
			trace_println('${@FN}: found match for (bool) ${fm.dbg_match(flag_ctx, field,
				'true', '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				pos:        pos
			}
			fm.handled_pos << pos
			return true
		}
	}

	if flag_name == field.match_name || flag_name == field.short {
		if field.hints.has(.is_array) {
			trace_println('${@FN}: found match for (V `flag.FlagParser` (long) style multiple occurrences) ${fm.dbg_match(flag_ctx,
				field, next, '')}')
			fm.array_field_map_flag[field.name] << FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(next)
				pos:        pos
			}
		} else {
			trace_println('${@FN}: found match for V (`flag.FlagParser` (long) style) ${fm.dbg_match(flag_ctx,
				field, next, '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(next)
				pos:        pos
			}
		}
		fm.handled_pos << pos
		fm.handled_pos << pos + 1 // arg
		return true
	}
	return false
}

// map_go_flag_short returns `true` if the GO short style flag in `flag_ctx` can be mapped to `field`.
// map_go_flag_short adds data of the match in the internal structures for further processing if applicable
fn (mut fm FlagMapper) map_go_flag_short(flag_ctx FlagContext, field StructField) !bool {
	flag_raw := flag_ctx.raw
	flag_name := flag_ctx.name
	pos := flag_ctx.pos
	used_delimiter := flag_ctx.delimiter
	next := flag_ctx.next

	if field.hints.has(.is_bool) {
		if flag_name == field.match_name {
			arg := if flag_raw.contains('=') { flag_raw.all_after('=') } else { '' }
			if arg != '' {
				return error('flag `${flag_raw}` can not be assigned to bool field "${field.name}"')
			}
			trace_println('${@FN}: found match for (bool) ${fm.dbg_match(flag_ctx, field,
				'true', '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				pos:        pos
			}
			fm.handled_pos << pos
			return true
		}
	}

	if flag_name == field.match_name || flag_name == field.short {
		if field.hints.has(.is_array) {
			trace_println('${@FN}: found match for (GO short style multiple occurrences) ${fm.dbg_match(flag_ctx,
				field, next, '')}')
			fm.array_field_map_flag[field.name] << FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(next)
				pos:        pos
			}
		} else {
			trace_println('${@FN}: found match for (GO short style) ${fm.dbg_match(flag_ctx,
				field, next, '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(next)
				pos:        pos
			}
		}
		fm.handled_pos << pos
		fm.handled_pos << pos + 1 // arg
		return true
	}
	return false
}

// map_go_flag_long returns `true` if the GO long style flag in `flag_ctx` can be mapped to `field`.
// map_go_flag_long adds data of the match in the internal structures for further processing if applicable
fn (mut fm FlagMapper) map_go_flag_long(flag_ctx FlagContext, field StructField) !bool {
	flag_raw := flag_ctx.raw
	flag_name := flag_ctx.name
	pos := flag_ctx.pos
	used_delimiter := flag_ctx.delimiter

	if flag_name == field.match_name {
		if field.hints.has(.is_bool) {
			arg := if flag_raw.contains('=') { flag_raw.all_after('=') } else { '' }
			if arg != '' {
				return error('flag `${flag_raw}` can not be assigned to bool field "${field.name}"')
			}
			trace_println('${@FN}: found match for (bool) (GO `flag` style) ${fm.dbg_match(flag_ctx,
				field, 'true', '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				pos:        pos
			}
			fm.handled_pos << pos
			return true
		}

		if !flag_raw.contains('=') {
			if field.hints.has(.is_int_type) && field.hints.has(.can_repeat) {
				return error('field `${field.name}` has @[repeats], only POSIX short style allows repeating')
			}
			return error('long delimiter `${used_delimiter}` flag `${flag_raw}` mapping to `${field.name}` in ${fm.config.style} style parsing mode, expects GO (GNU) style assignment. E.g.: --name=value')
		}

		arg := if flag_raw.contains('=') { flag_raw.all_after('=') } else { '' }
		if field.hints.has(.is_array) {
			trace_println('${@FN}: found match for (GO `flag` style multiple occurrences) ${fm.dbg_match(flag_ctx,
				field, arg, '')}')
			fm.array_field_map_flag[field.name] << FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(arg)
				pos:        pos
			}
		} else {
			trace_println('${@FN}: found match for (GO `flag` style) ${fm.dbg_match(flag_ctx,
				field, arg, '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(arg)
				pos:        pos
			}
		}
		fm.handled_pos << pos // NOTE: arg is part of the flag in GO (GNU) long style args
		return true
	}
	return false
}

// map_gnu_long returns `true` if the GNU (long) style flag in `flag_ctx` can be mapped to `field`.
// map_gnu_long adds data of the match in the internal structures for further processing if applicable
fn (mut fm FlagMapper) map_gnu_long(flag_ctx FlagContext, field StructField) !bool {
	flag_raw := flag_ctx.raw
	flag_name := flag_ctx.name
	pos := flag_ctx.pos
	used_delimiter := flag_ctx.delimiter

	if flag_name == field.match_name {
		if field.hints.has(.is_bool) {
			arg := if flag_raw.contains('=') { flag_raw.all_after('=') } else { '' }
			if arg != '' {
				return error('flag `${flag_raw}` can not be assigned to bool field "${field.name}"')
			}
			trace_println('${@FN}: found match for (bool) ${fm.dbg_match(flag_ctx, field,
				'true', '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				pos:        pos
			}
			fm.handled_pos << pos
			return true
		} else if fm.config.style in [.long, .short_long] && !flag_raw.contains('=') {
			if field.hints.has(.is_int_type) && field.hints.has(.can_repeat) {
				return error('field `${field.name}` has @[repeats], only POSIX short style allows repeating')
			}
			return error('long delimiter `${used_delimiter}` flag `${flag_raw}` mapping to `${field.name}` in ${fm.config.style} style parsing mode, expects GNU style assignment. E.g.: --name=value')
		}

		arg := if flag_raw.contains('=') { flag_raw.all_after('=') } else { '' }
		if field.hints.has(.is_array) {
			trace_println('${@FN}: found match for (GNU style multiple occurrences) ${fm.dbg_match(flag_ctx,
				field, arg, '')}')
			fm.array_field_map_flag[field.name] << FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(arg)
				pos:        pos
			}
		} else {
			trace_println('${@FN}: found match for (GNU style) ${fm.dbg_match(flag_ctx,
				field, arg, '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(arg)
				pos:        pos
			}
		}
		fm.handled_pos << pos // NOTE: arg is part of the flag in GNU long style args
		return true
	}
	return false
}

// map_posix_short_cluster looks for multiple combined short flags and optional
// tail arguments e.g. `-yxz( a)` = `-y -x -z( a)` and maps them to the respective struct fields.
// map_posix_short_cluster adds data of the match in the internal structures for further processing if applicable.
fn (mut fm FlagMapper) map_posix_short_cluster(flag_ctx FlagContext) ! {
	flag_name := flag_ctx.name
	if flag_name.len <= 1 {
		return
	}
	// Do not handle multiple `-vv`, `map_posix_short` does that
	if flag_name[0] == flag_name[1] {
		return
	}

	if flag_name.len > 1 {
		mut split := flag_name.split('')
		mut matched_fields := map[string]StructField{}
		for mflag in split {
			mut matched := false
			for _, field in fm.si.fields {
				if smatch_name := field.shortest_match_name() {
					if mflag == smatch_name {
						trace_println('${@FN}: cluster flag `${mflag}` matches field `${smatch_name}`')
						matched_fields[mflag] = field
						matched = true
					}
				}
			}
			if !matched {
				break
			}
		}

		if matched_fields.len == 0 {
			return
		}

		// Iterate `split` instead of `matched_fields` since the order of appearance has significance
		for i := 0; i < split.len; i++ {
			mflag := split[i]
			if field := matched_fields[mflag] {
				// trace_println('${@FN}: ${mflag} matches ${field.name}')
				mf := FlagData{
					raw:        flag_ctx.raw
					field_name: field.name
					delimiter:  flag_ctx.delimiter
					name:       mflag
					pos:        flag_ctx.pos
				}
				if field.hints.has(.is_bool) {
					trace_println('${@FN}: found match for (bool) ${fm.dbg_match(flag_ctx,
						field, 'true', '')}')
					fm.field_map_flag[mf.field_name] = mf
					fm.handled_pos << flag_ctx.pos
				} else {
					mut arg := split[i + 1..].clone().join('')
					mut next_is_used := false
					if arg == '' {
						arg = flag_ctx.next
						if arg != '' {
							next_is_used = true
						}
					}
					if field.hints.has(.is_array) {
						fm.array_field_map_flag[mf.field_name] << FlagData{
							...mf
							arg: ?string(arg)
						}
						trace_println('${@FN}: found match for (array) ${fm.dbg_match(flag_ctx,
							field, arg, '')}')
					} else {
						trace_println('${@FN}: found match for (other) ${fm.dbg_match(flag_ctx,
							field, arg, '')}')
						fm.field_map_flag[mf.field_name] = FlagData{
							...mf
							arg: ?string(arg)
						}
					}
					fm.handled_pos << flag_ctx.pos
					if next_is_used {
						fm.handled_pos << flag_ctx.pos + 1 // next
					}
					break
				}
			}
		}
	}
}

// map_posix_short returns `true` if the POSIX (short) style flag in `flag_ctx` can be mapped to `field`.
// map_posix_short adds data of the match in the internal structures for further processing if applicable
// map_posix_short handles, amoung other things the mapping of repeatable short flags. E.g.: `-vvv vvv`
fn (mut fm FlagMapper) map_posix_short(flag_ctx FlagContext, field StructField) !bool {
	flag_raw := flag_ctx.raw
	mut flag_name := flag_ctx.name
	pos := flag_ctx.pos
	used_delimiter := flag_ctx.delimiter
	mut next := flag_ctx.next
	struct_name := fm.si.name

	first_letter := flag_name.split('')[0]
	next_first_letter := if next != '' {
		next.split('')[0]
	} else {
		''
	}
	count_of_first_letter_repeats := flag_name.count(first_letter)
	count_of_next_first_letter_repeats := next.count(next_first_letter)

	if field.hints.has(.is_bool) {
		if flag_name == field.match_name {
			arg := if flag_raw.contains('=') { flag_raw.all_after('=') } else { '' }
			if arg != '' {
				return error('flag `${flag_raw}` can not be assigned to bool field "${field.name}"')
			}
			trace_println('${@FN}: found match for (bool) ${fm.dbg_match(flag_ctx, field,
				'true', '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				pos:        pos
			}
			fm.handled_pos << pos
			return true
		}

		if field.short == flag_name {
			trace_println('${@FN}: found match for (bool) ${fm.dbg_match(flag_ctx, field,
				'true', '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				pos:        pos
			}
			fm.handled_pos << pos
			return true
		}
	}

	if first_letter == field.short {
		// `-vvvvv`, `-vv vvv` or `-v vvvv`
		if field.hints.has(.can_repeat) {
			mut do_continue := false
			if count_of_first_letter_repeats == flag_name.len {
				trace_println('${@FN}: found match for (repeatable) ${fm.dbg_match(flag_ctx,
					field, 'true', '')}')
				fm.field_map_flag[field.name] = FlagData{
					raw:        flag_raw
					field_name: field.name
					delimiter:  used_delimiter
					name:       flag_name
					pos:        pos
					repeats:    count_of_first_letter_repeats
				}
				fm.handled_pos << pos
				do_continue = true

				if next_first_letter == first_letter
					&& count_of_next_first_letter_repeats == next.len {
					trace_println('${@FN}: field "${field.name}" allow repeats and ${flag_raw} ${next} repeats ${
						count_of_next_first_letter_repeats + count_of_first_letter_repeats} times (via argument)')
					fm.field_map_flag[field.name] = FlagData{
						raw:        flag_raw
						field_name: field.name
						delimiter:  used_delimiter
						name:       flag_name
						pos:        pos
						repeats:    count_of_next_first_letter_repeats +
							count_of_first_letter_repeats
					}
					fm.handled_pos << pos
					fm.handled_pos << pos + 1 // next
					do_continue = true
				} else {
					trace_println('${@FN}: field "${field.name}" allow repeats and ${flag_raw} repeats ${count_of_first_letter_repeats} times')
				}
				if do_continue {
					return true
				}
			}
		} else if field.hints.has(.is_array) {
			split := flag_name.trim_string_left(field.short)
			mut next_is_handled := true
			if split != '' {
				next = split
				flag_name = flag_name.trim_string_right(split)
				next_is_handled = false
			}

			if next == '' {
				return error('flag "${flag_raw}" expects an argument')
			}
			trace_println('${@FN}: found match for (multiple occurrences) ${fm.dbg_match(flag_ctx,
				field, next, '')}')

			fm.array_field_map_flag[field.name] << FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(next)
				pos:        pos
			}
			fm.handled_pos << pos
			if next_is_handled {
				fm.handled_pos << pos + 1 // next
			}
			return true
		} else if !flag_ctx.next.starts_with(used_delimiter) {
			if field.short == flag_name {
				trace_println('${@FN}: found match for (${field.type_name}) ${fm.dbg_match(flag_ctx,
					field, next, '')}')
				fm.field_map_flag[field.name] = FlagData{
					raw:        flag_raw
					field_name: field.name
					delimiter:  used_delimiter
					name:       flag_name
					arg:        ?string(next)
					pos:        pos
				}
				fm.handled_pos << pos
				fm.handled_pos << pos + 1 // next
				return true
			}
		}
	}
	if (fm.config.style == .short || field.hints.has(.short_only)) && first_letter == field.short {
		split := flag_name.trim_string_left(field.short)
		mut next_is_handled := true
		if split != '' {
			next = split
			flag_name = flag_name.trim_string_right(split)
			next_is_handled = false
		}

		if next == '' {
			return error('flag "${flag_raw}" expects an argument')
		}
		trace_println('${@FN}: found match for (short only) ${struct_name}.${field.name} (${field.match_name}) = ${field.short} = ${next}')

		fm.field_map_flag[field.name] = FlagData{
			raw:        flag_raw
			field_name: field.name
			delimiter:  used_delimiter
			name:       flag_name
			arg:        ?string(next)
			pos:        pos
			repeats:    count_of_first_letter_repeats
		}
		fm.handled_pos << pos
		if next_is_handled {
			fm.handled_pos << pos + 1 // next
		}
		return true
	} else if flag_name == field.match_name && !(field.hints.has(.short_only)
		&& flag_name == field.short) {
		trace_println('${@FN}: found match for (repeats) ${fm.dbg_match(flag_ctx, field,
			next, '')}')
		if next == '' {
			return error('flag "${flag_raw}" expects an argument')
		}
		fm.field_map_flag[field.name] = FlagData{
			raw:        flag_raw
			field_name: field.name
			delimiter:  used_delimiter
			name:       flag_name
			arg:        ?string(next)
			pos:        pos
			repeats:    count_of_first_letter_repeats
		}
		fm.handled_pos << pos
		fm.handled_pos << pos + 1 // next
		return true
	}
	return false
}

// map_cmd_exe returns `true` if the CMD.EXE style flag in `flag_ctx` can be mapped to `field`.
// map_cmd_exe adds data of the match in the internal structures for further processing if applicable
fn (mut fm FlagMapper) map_cmd_exe(flag_ctx FlagContext, field StructField) !bool {
	flag_raw := flag_ctx.raw
	flag_name := flag_ctx.name
	pos := flag_ctx.pos
	used_delimiter := flag_ctx.delimiter
	next := flag_ctx.next

	if flag_name == field.match_name {
		if field.hints.has(.is_bool) {
			trace_println('${@FN}: found (long) match for (bool) (CMD.EXE style) ${fm.dbg_match(flag_ctx,
				field, 'true', '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				pos:        pos
			}
			fm.handled_pos << pos
			return true
		}
		// Not sure original CMD.EXE flags supported multiple flags with same name??
		if field.hints.has(.is_array) {
			trace_println('${@FN}: found match for (CMD.EXE style multiple occurrences) ${fm.dbg_match(flag_ctx,
				field, next, '')}')
			fm.array_field_map_flag[field.name] << FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(next)
				pos:        pos
			}
		} else {
			trace_println('${@FN}: found match for (CMD.EXE style) ${fm.dbg_match(flag_ctx,
				field, next, '')}')
			fm.field_map_flag[field.name] = FlagData{
				raw:        flag_raw
				field_name: field.name
				delimiter:  used_delimiter
				name:       flag_name
				arg:        ?string(next)
				pos:        pos
			}
		}
		fm.handled_pos << pos
		fm.handled_pos << pos + 1 // arg
		return true
	}
	// Not aware of CMD.EXE flags longer than one (ASCII??) character
	if shortest_match_name := field.shortest_match_name() {
		if flag_name == shortest_match_name {
			if field.hints.has(.is_bool) {
				trace_println('${@FN}: found match for (bool) (CMD.EXE style) ${fm.dbg_match(flag_ctx,
					field, 'true', '')}')
				fm.field_map_flag[field.name] = FlagData{
					raw:        flag_raw
					field_name: field.name
					delimiter:  used_delimiter
					name:       flag_name
					pos:        pos
				}
				fm.handled_pos << pos
				return true
			}

			// Not sure original CMD.EXE flags supported multiple flags with same name??
			if field.hints.has(.is_array) {
				trace_println('${@FN}: found match for (CMD.EXE style multiple occurrences) ${fm.dbg_match(flag_ctx,
					field, next, '')}')
				fm.array_field_map_flag[field.name] << FlagData{
					raw:        flag_raw
					field_name: field.name
					delimiter:  used_delimiter
					name:       flag_name
					arg:        ?string(next)
					pos:        pos
				}
			} else {
				trace_println('${@FN}: found match for (CMD.EXE style) ${fm.dbg_match(flag_ctx,
					field, next, '')}')
				fm.field_map_flag[field.name] = FlagData{
					raw:        flag_raw
					field_name: field.name
					delimiter:  used_delimiter
					name:       flag_name
					arg:        ?string(next)
					pos:        pos
				}
			}
			fm.handled_pos << pos
			fm.handled_pos << pos + 1 // arg
			return true
		}
	}
	return false
}
