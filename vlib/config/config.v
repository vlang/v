module config

import os
import toml
import strconv

// LoadOptions selects where configuration values come from, in precedence
// order: cli, then env, then files (first hit wins), then defaults.
pub struct LoadOptions {
pub:
	// cli are raw arguments, e.g. os.args[1..]; understands --key=value,
	// --key value, and bare --flag (meaning "true")
	cli []string
	// env_prefix is prepended to UPPER(field) names, e.g. 'APP_'
	env_prefix string
	// files are TOML documents read in order; unreadable paths are errors
	files []string
}

// ConfigError is a structured configuration failure. It names the setting
// (dotted path, e.g. `db.host`), the source that provided it (`cli`, `env`,
// `file`, `default`, or `missing`), what was expected, and why it failed.
// Values of @[secret] fields are never interpolated into reason.
pub struct ConfigError {
pub:
	setting  string
	source   string
	expected string
	reason   string
}

// msg returns the formatted diagnostic.
pub fn (e ConfigError) msg() string {
	return 'config: `${e.setting}` [${e.source}]: expected ${e.expected}: ${e.reason}'
}

// code always returns 1 so ConfigError satisfies IError.
pub fn (e ConfigError) code() int {
	return 1
}

// load fills a struct from the configured sources. Each field takes its
// value from the first source that provides it:
//
//  1. cli: `--field` or `--field-with-dashes`, as `--k=v` or `--k v`.
//     Nested fields use `--parent-child`; `--parent.child` also works.
//     Arrays use comma-separated values, e.g. `--tags=a,b`.
//     Maps use comma-separated pairs, e.g. `--limits=cpu=2,mem=4G`.
//  2. env: `@[env: 'NAME']`, else `env_prefix + UPPER(field)`.
//     Nested fields use `PREFIX_PARENT_CHILD`, e.g. `APP_DB_HOST`.
//     Arrays and maps use the same comma-separated syntax as cli.
//  3. files: the TOML top-level key matching the field name. Nested structs
//     read TOML tables via dotted paths (e.g. `db.host`); arrays and maps
//     read native TOML arrays and tables.
//  4. defaults: native `= value` initializers first, then `@[default: '...']`.
//     When no source provides the key, a non-zero native initializer wins;
//     otherwise the `@[default]` string is parsed with the same rules.
//
// A field with no value and no usable default is an error, as is a field
// marked `@[required]`. `@[secret]` marks sensitive fields: their values are
// redacted from error output (see redacted) and never interpolated into
// diagnostics. Supported field types are string, int, bool, u16, i64, f64,
// nested structs, and arrays/maps of those primitives; anything else is a
// compile error. Example:
//
// ```v
// struct ServerConfig {
// 	host  string @[env: 'HOST'; default: 'localhost']
// 	port  int    @[default: '8080']
// 	debug bool   @[default: 'false']
// 	token string @[required; secret]
// }
//
// cfg := config.load[ServerConfig](cli: os.args[1..], env_prefix: 'APP_',
// 	files: ['app.toml', '/etc/app.toml'])!
// ```
pub fn load[T](opts LoadOptions) !T {
	cli := parse_cli_args(opts.cli)
	mut cfg := T{}
	contains_secret := has_secret_value(cfg)
	mut docs := []toml.Doc{}
	for path in opts.files {
		docs << toml.parse_file(path) or {
			if contains_secret && os.is_file(path) {
				return error('config: cannot read `${path}`: invalid TOML (details redacted: config contains @[secret] fields)')
			}
			return error('config: cannot read `${path}`: ${err.msg()}')
		}
	}
	$if T is $struct {
		load_into(mut cfg, '', '', opts.env_prefix, cli, docs, false)!
	} $else {
		$compile_error('config.load: expected struct, use config.load[MyStruct](...)')
	}
	return cfg
}

// redacted returns a debug representation of cfg with @[secret] fields
// (and secret subtrees) replaced by '***'. Nested structs are rendered
// recursively so inner secrets stay masked.
pub fn redacted[T](cfg T) string {
	$if T is $struct {
		mut parts := []string{}
		$for field in T.fields {
			fname := field.name
			$if field.is_struct {
				if has_attr(field.attrs, 'secret') {
					parts << '${fname}: ' + "'***'"
				} else {
					nested := cfg.$(field.name)
					parts << fname + ': ' + redacted(nested)
				}
			} $else {
				if has_attr(field.attrs, 'secret') {
					parts << '${fname}: ' + "'***'"
				} else {
					val := cfg.$(field.name)
					parts << fname + ': ' + '${val}'
				}
			}
		}
		return T.name + '{' + parts.join(', ') + '}'
	} $else {
		$compile_error('config.redacted: expected struct')
	}
}

// load_into fills cfg from the configured sources. cli_prefix and
// toml_prefix locate nested fields (`db_` / `db.`); env_prefix is the current
// environment base (already including parent segments); parent_secret marks
// the whole subtree as sensitive.
fn load_into[T](mut cfg T, cli_prefix string, toml_prefix string, env_prefix string, cli map[string]string, docs []toml.Doc, parent_secret bool) ! {
	$for field in T.fields {
		fname := field.name
		full_name := cli_prefix + fname
		toml_key := toml_prefix + fname
		is_secret := parent_secret || has_attr(field.attrs, 'secret')
		$if field.typ is string {
			cfg.$(field.name) = load_string_field(cfg.$(field.name), full_name,
				toml_key, field.attrs, env_prefix, cli, docs)!
		} $else $if field.typ is int {
			cfg.$(field.name) = load_int_field(cfg.$(field.name), full_name,
				toml_key, field.attrs, env_prefix, cli, docs, is_secret)!
		} $else $if field.typ is bool {
			cfg.$(field.name) = load_bool_field(cfg.$(field.name), full_name,
				toml_key, field.attrs, env_prefix, cli, docs, is_secret)!
		} $else $if field.typ is u16 {
			cfg.$(field.name) = load_u16_field(cfg.$(field.name), full_name,
				toml_key, field.attrs, env_prefix, cli, docs, is_secret)!
		} $else $if field.typ is i64 {
			cfg.$(field.name) = load_i64_field(cfg.$(field.name), full_name,
				toml_key, field.attrs, env_prefix, cli, docs, is_secret)!
		} $else $if field.typ is f64 {
			cfg.$(field.name) = load_f64_field(cfg.$(field.name), full_name,
				toml_key, field.attrs, env_prefix, cli, docs, is_secret)!
		} $else $if field.is_array {
			cfg.$(field.name) = load_array_field(cfg.$(field.name), full_name,
				toml_key, field.attrs, env_prefix, cli, docs, is_secret)!
		} $else $if field.is_map {
			cfg.$(field.name) = load_map_field(cfg.$(field.name), full_name,
				toml_key, field.attrs, env_prefix, cli, docs, is_secret)!
		} $else $if field.is_struct {
			if has_attr(field.attrs, 'default') {
				return ConfigError{
					setting:  toml_key
					source:   'default'
					expected: 'struct'
					reason:   'structs do not accept @[default]; set defaults on fields or use native initializers'
				}
			}
			child_cli_prefix := full_name + '_'
			child_toml_prefix := toml_key + '.'
			mut child_env_prefix := env_prefix + fname.to_upper() + '_'
			if custom := attr_arg(field.attrs, 'env') {
				child_env_prefix = custom
				if !child_env_prefix.ends_with('_') {
					child_env_prefix += '_'
				}
			}
			if has_attr(field.attrs, 'required')
				&& !subtree_provided(full_name, toml_key, child_env_prefix, cli, docs) {
				return ConfigError{
					setting:  toml_key
					source:   'missing'
					expected: 'struct'
					reason:   'required key was not provided'
				}
			}
			for doc in docs {
				if v := doc.value_opt(toml_key) {
					if v is map[string]toml.Any {
						continue
					}
					return ConfigError{
						setting:  toml_key
						source:   'file'
						expected: 'struct'
						reason:   'expected a table value'
					}
				}
			}
			mut nested := cfg.$(field.name)
			load_into(mut nested, child_cli_prefix, child_toml_prefix, child_env_prefix,
				cli, docs, is_secret)!
			cfg.$(field.name) = nested
		} $else {
			$compile_error('config: unsupported field type; use string, int, bool, u16, i64, f64, nested structs, arrays or maps of supported primitives')
		}
	}
}

// subtree_provided reports whether any source provides a value under the
// nested table at toml_parent_key.
fn subtree_provided(cli_full_name string, toml_parent_key string, child_env_prefix string, cli map[string]string, docs []toml.Doc) bool {
	cli_base := '--' + cli_full_name.replace('_', '-')
	for k in cli.keys() {
		if k == cli_base || k.starts_with(cli_base + '-') || k.starts_with(cli_base + '.') {
			return true
		}
	}
	dot_cli := ('--' + toml_parent_key).replace('_', '-')
	for k in cli.keys() {
		if k == dot_cli || k.starts_with(dot_cli + '-') || k.starts_with(dot_cli + '.') {
			return true
		}
	}
	for k in os.environ().keys() {
		if k.starts_with(child_env_prefix) {
			return true
		}
	}
	for doc in docs {
		if _ := doc.value_opt(toml_parent_key) {
			return true
		}
	}
	return false
}

// any_to_scalar_string renders a TOML scalar for string coercion.
fn any_to_scalar_string(v toml.Any) string {
	match v {
		bool {
			return v.str()
		}
		f32 {
			return v.str()
		}
		f64 {
			return v.str()
		}
		i64 {
			return v.str()
		}
		int {
			return v.str()
		}
		u64 {
			return v.str()
		}
		else {
			return v.string()
		}
	}
}

// load_string_field resolves one string field.
fn load_string_field(current string, full_name string, toml_key string, attrs []string, env_prefix string, cli map[string]string, docs []toml.Doc) !string {
	if v := cli['--' + full_name.replace('_', '-')] {
		return v
	}
	if v := cli['--' + toml_key.replace('_', '-')] {
		return v
	}
	env_name := attr_arg(attrs, 'env') or { env_prefix + toml_key.all_after_last('.').to_upper() }
	if v := os.getenv_opt(env_name) {
		return v
	}
	for doc in docs {
		if v := doc.value_opt(toml_key) {
			match v {
				[]toml.Any, map[string]toml.Any {
					return ConfigError{
						setting:  toml_key
						source:   'file'
						expected: 'string'
						reason:   'expected a string value'
					}
				}
				else {
					return any_to_scalar_string(v)
				}
			}
		}
	}
	if current != '' {
		return current
	}
	if d := attr_arg(attrs, 'default') {
		return d
	}
	if has_attr(attrs, 'required') {
		return ConfigError{
			setting:  toml_key
			source:   'missing'
			expected: 'string'
			reason:   'required key was not provided'
		}
	}
	return ConfigError{
		setting:  toml_key
		source:   'missing'
		expected: 'string'
		reason:   'no value provided and no default'
	}
}

// load_int_field resolves one int field.
fn load_int_field(current int, full_name string, toml_key string, attrs []string, env_prefix string, cli map[string]string, docs []toml.Doc, is_secret bool) !int {
	if v := cli['--' + full_name.replace('_', '-')] {
		return parse_config_int(toml_key, v, 'cli', is_secret)!
	}
	if v := cli['--' + toml_key.replace('_', '-')] {
		return parse_config_int(toml_key, v, 'cli', is_secret)!
	}
	env_name := attr_arg(attrs, 'env') or { env_prefix + toml_key.all_after_last('.').to_upper() }
	if v := os.getenv_opt(env_name) {
		return parse_config_int(toml_key, v, 'env', is_secret)!
	}
	for doc in docs {
		if v := doc.value_opt(toml_key) {
			match v {
				[]toml.Any, map[string]toml.Any {
					return ConfigError{
						setting:  toml_key
						source:   'file'
						expected: 'int'
						reason:   'expected an integer value'
					}
				}
				else {
					return parse_config_int(toml_key, any_to_scalar_string(v),
						'file', is_secret)!
				}
			}
		}
	}
	if current != 0 {
		return current
	}
	if d := attr_arg(attrs, 'default') {
		return parse_config_int(toml_key, d, 'default', is_secret)!
	}
	if has_attr(attrs, 'required') {
		return ConfigError{
			setting:  toml_key
			source:   'missing'
			expected: 'int'
			reason:   'required key was not provided'
		}
	}
	return ConfigError{
		setting:  toml_key
		source:   'missing'
		expected: 'int'
		reason:   'no value provided and no default'
	}
}

// load_bool_field resolves one bool field.
fn load_bool_field(current bool, full_name string, toml_key string, attrs []string, env_prefix string, cli map[string]string, docs []toml.Doc, is_secret bool) !bool {
	if v := cli['--' + full_name.replace('_', '-')] {
		return parse_config_bool(toml_key, v, 'cli', is_secret)!
	}
	if v := cli['--' + toml_key.replace('_', '-')] {
		return parse_config_bool(toml_key, v, 'cli', is_secret)!
	}
	env_name := attr_arg(attrs, 'env') or { env_prefix + toml_key.all_after_last('.').to_upper() }
	if v := os.getenv_opt(env_name) {
		return parse_config_bool(toml_key, v, 'env', is_secret)!
	}
	for doc in docs {
		if v := doc.value_opt(toml_key) {
			match v {
				[]toml.Any, map[string]toml.Any {
					return ConfigError{
						setting:  toml_key
						source:   'file'
						expected: 'bool'
						reason:   'expected a boolean value'
					}
				}
				else {
					return parse_config_bool(toml_key, any_to_scalar_string(v),
						'file', is_secret)!
				}
			}
		}
	}
	if current {
		return current
	}
	if d := attr_arg(attrs, 'default') {
		return parse_config_bool(toml_key, d, 'default', is_secret)!
	}
	if has_attr(attrs, 'required') {
		return ConfigError{
			setting:  toml_key
			source:   'missing'
			expected: 'bool'
			reason:   'required key was not provided'
		}
	}
	return ConfigError{
		setting:  toml_key
		source:   'missing'
		expected: 'bool'
		reason:   'no value provided and no default'
	}
}

// load_u16_field resolves one u16 field.
fn load_u16_field(current u16, full_name string, toml_key string, attrs []string, env_prefix string, cli map[string]string, docs []toml.Doc, is_secret bool) !u16 {
	if v := cli['--' + full_name.replace('_', '-')] {
		return parse_config_u16(toml_key, v, 'cli', is_secret)!
	}
	if v := cli['--' + toml_key.replace('_', '-')] {
		return parse_config_u16(toml_key, v, 'cli', is_secret)!
	}
	env_name := attr_arg(attrs, 'env') or { env_prefix + toml_key.all_after_last('.').to_upper() }
	if v := os.getenv_opt(env_name) {
		return parse_config_u16(toml_key, v, 'env', is_secret)!
	}
	for doc in docs {
		if v := doc.value_opt(toml_key) {
			match v {
				[]toml.Any, map[string]toml.Any {
					return ConfigError{
						setting:  toml_key
						source:   'file'
						expected: 'u16'
						reason:   'expected an integer value'
					}
				}
				else {
					return parse_config_u16(toml_key, any_to_scalar_string(v),
						'file', is_secret)!
				}
			}
		}
	}
	if current != 0 {
		return current
	}
	if d := attr_arg(attrs, 'default') {
		return parse_config_u16(toml_key, d, 'default', is_secret)!
	}
	if has_attr(attrs, 'required') {
		return ConfigError{
			setting:  toml_key
			source:   'missing'
			expected: 'u16'
			reason:   'required key was not provided'
		}
	}
	return ConfigError{
		setting:  toml_key
		source:   'missing'
		expected: 'u16'
		reason:   'no value provided and no default'
	}
}

// load_i64_field resolves one i64 field.
fn load_i64_field(current i64, full_name string, toml_key string, attrs []string, env_prefix string, cli map[string]string, docs []toml.Doc, is_secret bool) !i64 {
	if v := cli['--' + full_name.replace('_', '-')] {
		return parse_config_i64(toml_key, v, 'cli', is_secret)!
	}
	if v := cli['--' + toml_key.replace('_', '-')] {
		return parse_config_i64(toml_key, v, 'cli', is_secret)!
	}
	env_name := attr_arg(attrs, 'env') or { env_prefix + toml_key.all_after_last('.').to_upper() }
	if v := os.getenv_opt(env_name) {
		return parse_config_i64(toml_key, v, 'env', is_secret)!
	}
	for doc in docs {
		if v := doc.value_opt(toml_key) {
			match v {
				[]toml.Any, map[string]toml.Any {
					return ConfigError{
						setting:  toml_key
						source:   'file'
						expected: 'i64'
						reason:   'expected an integer value'
					}
				}
				else {
					return parse_config_i64(toml_key, any_to_scalar_string(v),
						'file', is_secret)!
				}
			}
		}
	}
	if current != 0 {
		return current
	}
	if d := attr_arg(attrs, 'default') {
		return parse_config_i64(toml_key, d, 'default', is_secret)!
	}
	if has_attr(attrs, 'required') {
		return ConfigError{
			setting:  toml_key
			source:   'missing'
			expected: 'i64'
			reason:   'required key was not provided'
		}
	}
	return ConfigError{
		setting:  toml_key
		source:   'missing'
		expected: 'i64'
		reason:   'no value provided and no default'
	}
}

// load_f64_field resolves one f64 field.
fn load_f64_field(current f64, full_name string, toml_key string, attrs []string, env_prefix string, cli map[string]string, docs []toml.Doc, is_secret bool) !f64 {
	if v := cli['--' + full_name.replace('_', '-')] {
		return parse_config_f64(toml_key, v, 'cli', is_secret)!
	}
	if v := cli['--' + toml_key.replace('_', '-')] {
		return parse_config_f64(toml_key, v, 'cli', is_secret)!
	}
	env_name := attr_arg(attrs, 'env') or { env_prefix + toml_key.all_after_last('.').to_upper() }
	if v := os.getenv_opt(env_name) {
		return parse_config_f64(toml_key, v, 'env', is_secret)!
	}
	for doc in docs {
		if v := doc.value_opt(toml_key) {
			match v {
				[]toml.Any, map[string]toml.Any {
					return ConfigError{
						setting:  toml_key
						source:   'file'
						expected: 'f64'
						reason:   'expected a float value'
					}
				}
				else {
					return parse_config_f64(toml_key, any_to_scalar_string(v),
						'file', is_secret)!
				}
			}
		}
	}
	if current != 0.0 {
		return current
	}
	if d := attr_arg(attrs, 'default') {
		return parse_config_f64(toml_key, d, 'default', is_secret)!
	}
	if has_attr(attrs, 'required') {
		return ConfigError{
			setting:  toml_key
			source:   'missing'
			expected: 'f64'
			reason:   'required key was not provided'
		}
	}
	return ConfigError{
		setting:  toml_key
		source:   'missing'
		expected: 'f64'
		reason:   'no value provided and no default'
	}
}

// load_array_field resolves one array field of supported primitives.
// Cli, env and @[default] values are comma-separated; files read native
// TOML arrays. An empty string means an empty array.
fn load_array_field[T](current []T, full_name string, toml_key string, attrs []string, env_prefix string, cli map[string]string, docs []toml.Doc, is_secret bool) ![]T {
	$if T is string || T is bool || T is int || T is u16 || T is i64 || T is f64 {
		if v := cli['--' + full_name.replace('_', '-')] {
			return parse_array_string[T](v, toml_key, 'cli', is_secret)!
		}
		if v := cli['--' + toml_key.replace('_', '-')] {
			return parse_array_string[T](v, toml_key, 'cli', is_secret)!
		}
		env_name := attr_arg(attrs, 'env') or { env_prefix + toml_key.all_after_last('.').to_upper() }
		if v := os.getenv_opt(env_name) {
			return parse_array_string[T](v, toml_key, 'env', is_secret)!
		}
		for doc in docs {
			if v := doc.value_opt(toml_key) {
				return decode_any_array[T](v, toml_key, is_secret)!
			}
		}
		if current.len > 0 {
			return current
		}
		if d := attr_arg(attrs, 'default') {
			return parse_array_string[T](d, toml_key, 'default', is_secret)!
		}
		if has_attr(attrs, 'required') {
			return ConfigError{
				setting:  toml_key
				source:   'missing'
				expected: 'array'
				reason:   'required key was not provided'
			}
		}
		return ConfigError{
			setting:  toml_key
			source:   'missing'
			expected: 'array'
			reason:   'no value provided and no default'
		}
	} $else {
		$compile_error('config: unsupported array element type; use arrays of string, int, bool, u16, i64 or f64')
	}
}

// load_map_field resolves one map field with string keys and primitive
// values. Cli, env and @[default] values use `k=v,k2=v2` syntax; files read
// native TOML tables. An empty string means an empty map.
fn load_map_field[K, V](current map[K]V, full_name string, toml_key string, attrs []string, env_prefix string, cli map[string]string, docs []toml.Doc, is_secret bool) !map[K]V {
	$if K is string {
		$if V is string || V is bool || V is int || V is u16 || V is i64 || V is f64 {
			if v := cli['--' + full_name.replace('_', '-')] {
				return parse_map_string[K, V](v, toml_key, 'cli', is_secret)!
			}
			if v := cli['--' + toml_key.replace('_', '-')] {
				return parse_map_string[K, V](v, toml_key, 'cli', is_secret)!
			}
			env_name := attr_arg(attrs, 'env') or { env_prefix + toml_key.all_after_last('.').to_upper() }
			if v := os.getenv_opt(env_name) {
				return parse_map_string[K, V](v, toml_key, 'env', is_secret)!
			}
			for doc in docs {
				if v := doc.value_opt(toml_key) {
					return decode_any_map[K, V](v, toml_key, is_secret)!
				}
			}
			if current.len > 0 {
				return current
			}
			if d := attr_arg(attrs, 'default') {
				return parse_map_string[K, V](d, toml_key, 'default', is_secret)!
			}
			if has_attr(attrs, 'required') {
				return ConfigError{
					setting:  toml_key
					source:   'missing'
					expected: 'map'
					reason:   'required key was not provided'
				}
			}
			return ConfigError{
				setting:  toml_key
				source:   'missing'
				expected: 'map'
				reason:   'no value provided and no default'
			}
		} $else {
			$compile_error('config: unsupported map value type; use maps of string, int, bool, u16, i64 or f64')
		}
	} $else {
		$compile_error('config: unsupported map key type; use map[string]T with a supported primitive T')
	}
}

// parse_array_string parses comma-separated array values.
fn parse_array_string[T](raw string, setting string, source string, is_secret bool) ![]T {
	$if T is string {
		if raw == '' {
			return []T{}
		}
		parts := raw.split(',')
		mut out := []T{cap: parts.len}
		for part in parts {
			out << T(part.trim_space())
		}
		return out
	} $else $if T is bool {
		if raw == '' {
			return []T{}
		}
		parts := raw.split(',')
		mut out := []T{cap: parts.len}
		for i, part in parts {
			out << T(parse_config_bool('${setting}[${i}]', part.trim_space(),
				source, is_secret)!)
		}
		return out
	} $else $if T is int {
		if raw == '' {
			return []T{}
		}
		parts := raw.split(',')
		mut out := []T{cap: parts.len}
		for i, part in parts {
			out << T(parse_config_int('${setting}[${i}]', part.trim_space(),
				source, is_secret)!)
		}
		return out
	} $else $if T is u16 {
		if raw == '' {
			return []T{}
		}
		parts := raw.split(',')
		mut out := []T{cap: parts.len}
		for i, part in parts {
			out << T(parse_config_u16('${setting}[${i}]', part.trim_space(),
				source, is_secret)!)
		}
		return out
	} $else $if T is i64 {
		if raw == '' {
			return []T{}
		}
		parts := raw.split(',')
		mut out := []T{cap: parts.len}
		for i, part in parts {
			out << T(parse_config_i64('${setting}[${i}]', part.trim_space(),
				source, is_secret)!)
		}
		return out
	} $else $if T is f64 {
		if raw == '' {
			return []T{}
		}
		parts := raw.split(',')
		mut out := []T{cap: parts.len}
		for i, part in parts {
			out << T(parse_config_f64('${setting}[${i}]', part.trim_space(),
				source, is_secret)!)
		}
		return out
	} $else {
		$compile_error('config: unsupported array element type; use arrays of string, int, bool, u16, i64 or f64')
	}
}

// decode_any_array decodes a TOML array value.
fn decode_any_array[T](any toml.Any, setting string, is_secret bool) ![]T {
	$if T is string {
		values := if any is []toml.Any {
			any
		} else {
			return ConfigError{
				setting:  setting
				source:   'file'
				expected: 'array'
				reason:   'expected an array value'
			}
		}
		mut out := []T{cap: values.len}
		for i, item in values {
			if item is []toml.Any || item is map[string]toml.Any {
				return ConfigError{
					setting:  '${setting}[${i}]'
					source:   'file'
					expected: 'string'
					reason:   'expected a string value'
				}
			}
			out << T(any_to_scalar_string(item))
		}
		return out
	} $else $if T is bool {
		values := if any is []toml.Any {
			any
		} else {
			return ConfigError{
				setting:  setting
				source:   'file'
				expected: 'array'
				reason:   'expected an array value'
			}
		}
		mut out := []T{cap: values.len}
		for i, item in values {
			if item is []toml.Any || item is map[string]toml.Any {
				return ConfigError{
					setting:  '${setting}[${i}]'
					source:   'file'
					expected: 'bool'
					reason:   'expected a boolean value'
				}
			}
			out << T(parse_config_bool('${setting}[${i}]', any_to_scalar_string(item),
				'file', is_secret)!)
		}
		return out
	} $else $if T is int {
		values := if any is []toml.Any {
			any
		} else {
			return ConfigError{
				setting:  setting
				source:   'file'
				expected: 'array'
				reason:   'expected an array value'
			}
		}
		mut out := []T{cap: values.len}
		for i, item in values {
			if item is []toml.Any || item is map[string]toml.Any {
				return ConfigError{
					setting:  '${setting}[${i}]'
					source:   'file'
					expected: 'int'
					reason:   'expected an integer value'
				}
			}
			out << T(parse_config_int('${setting}[${i}]', any_to_scalar_string(item),
				'file', is_secret)!)
		}
		return out
	} $else $if T is u16 {
		values := if any is []toml.Any {
			any
		} else {
			return ConfigError{
				setting:  setting
				source:   'file'
				expected: 'array'
				reason:   'expected an array value'
			}
		}
		mut out := []T{cap: values.len}
		for i, item in values {
			if item is []toml.Any || item is map[string]toml.Any {
				return ConfigError{
					setting:  '${setting}[${i}]'
					source:   'file'
					expected: 'u16'
					reason:   'expected an integer value'
				}
			}
			out << T(parse_config_u16('${setting}[${i}]', any_to_scalar_string(item),
				'file', is_secret)!)
		}
		return out
	} $else $if T is i64 {
		values := if any is []toml.Any {
			any
		} else {
			return ConfigError{
				setting:  setting
				source:   'file'
				expected: 'array'
				reason:   'expected an array value'
			}
		}
		mut out := []T{cap: values.len}
		for i, item in values {
			if item is []toml.Any || item is map[string]toml.Any {
				return ConfigError{
					setting:  '${setting}[${i}]'
					source:   'file'
					expected: 'i64'
					reason:   'expected an integer value'
				}
			}
			out << T(parse_config_i64('${setting}[${i}]', any_to_scalar_string(item),
				'file', is_secret)!)
		}
		return out
	} $else $if T is f64 {
		values := if any is []toml.Any {
			any
		} else {
			return ConfigError{
				setting:  setting
				source:   'file'
				expected: 'array'
				reason:   'expected an array value'
			}
		}
		mut out := []T{cap: values.len}
		for i, item in values {
			if item is []toml.Any || item is map[string]toml.Any {
				return ConfigError{
					setting:  '${setting}[${i}]'
					source:   'file'
					expected: 'f64'
					reason:   'expected a float value'
				}
			}
			out << T(parse_config_f64('${setting}[${i}]', any_to_scalar_string(item),
				'file', is_secret)!)
		}
		return out
	} $else {
		$compile_error('config: unsupported array element type; use arrays of string, int, bool, u16, i64 or f64')
	}
}

// parse_map_string parses `k=v,k2=v2` map values.
fn parse_map_string[K, V](raw string, setting string, source string, is_secret bool) !map[K]V {
	$if K is string {
		$if V is string {
			mut out := map[K]V{}
			if raw == '' {
				return out
			}
			for pair in raw.split(',') {
				idx := pair.index('=') or {
					reason := if is_secret {
						'not a valid map entry (value redacted)'
					} else {
						'not a valid map entry: `${pair.trim_space()}`'
					}
					return ConfigError{
						setting:  setting
						source:   source
						expected: 'map'
						reason:   reason
					}
				}
				k := pair[..idx].trim_space()
				v := pair[idx + 1..].trim_space()
				out[K(k)] = V(v)
			}
			return out
		} $else $if V is bool {
			mut out := map[K]V{}
			if raw == '' {
				return out
			}
			for pair in raw.split(',') {
				idx := pair.index('=') or {
					reason := if is_secret {
						'not a valid map entry (value redacted)'
					} else {
						'not a valid map entry: `${pair.trim_space()}`'
					}
					return ConfigError{
						setting:  setting
						source:   source
						expected: 'map'
						reason:   reason
					}
				}
				k := pair[..idx].trim_space()
				v := pair[idx + 1..].trim_space()
				out[K(k)] = V(parse_config_bool(map_entry_setting(setting, k, is_secret), v, source,
					is_secret)!)
			}
			return out
		} $else $if V is int {
			mut out := map[K]V{}
			if raw == '' {
				return out
			}
			for pair in raw.split(',') {
				idx := pair.index('=') or {
					reason := if is_secret {
						'not a valid map entry (value redacted)'
					} else {
						'not a valid map entry: `${pair.trim_space()}`'
					}
					return ConfigError{
						setting:  setting
						source:   source
						expected: 'map'
						reason:   reason
					}
				}
				k := pair[..idx].trim_space()
				v := pair[idx + 1..].trim_space()
				out[K(k)] = V(parse_config_int(map_entry_setting(setting, k, is_secret), v, source,
					is_secret)!)
			}
			return out
		} $else $if V is u16 {
			mut out := map[K]V{}
			if raw == '' {
				return out
			}
			for pair in raw.split(',') {
				idx := pair.index('=') or {
					reason := if is_secret {
						'not a valid map entry (value redacted)'
					} else {
						'not a valid map entry: `${pair.trim_space()}`'
					}
					return ConfigError{
						setting:  setting
						source:   source
						expected: 'map'
						reason:   reason
					}
				}
				k := pair[..idx].trim_space()
				v := pair[idx + 1..].trim_space()
				out[K(k)] = V(parse_config_u16(map_entry_setting(setting, k, is_secret), v, source,
					is_secret)!)
			}
			return out
		} $else $if V is i64 {
			mut out := map[K]V{}
			if raw == '' {
				return out
			}
			for pair in raw.split(',') {
				idx := pair.index('=') or {
					reason := if is_secret {
						'not a valid map entry (value redacted)'
					} else {
						'not a valid map entry: `${pair.trim_space()}`'
					}
					return ConfigError{
						setting:  setting
						source:   source
						expected: 'map'
						reason:   reason
					}
				}
				k := pair[..idx].trim_space()
				v := pair[idx + 1..].trim_space()
				out[K(k)] = V(parse_config_i64(map_entry_setting(setting, k, is_secret), v, source,
					is_secret)!)
			}
			return out
		} $else $if V is f64 {
			mut out := map[K]V{}
			if raw == '' {
				return out
			}
			for pair in raw.split(',') {
				idx := pair.index('=') or {
					reason := if is_secret {
						'not a valid map entry (value redacted)'
					} else {
						'not a valid map entry: `${pair.trim_space()}`'
					}
					return ConfigError{
						setting:  setting
						source:   source
						expected: 'map'
						reason:   reason
					}
				}
				k := pair[..idx].trim_space()
				v := pair[idx + 1..].trim_space()
				out[K(k)] = V(parse_config_f64(map_entry_setting(setting, k, is_secret), v, source,
					is_secret)!)
			}
			return out
		} $else {
			$compile_error('config: unsupported map value type; use maps of string, int, bool, u16, i64 or f64')
		}
	} $else {
		$compile_error('config: unsupported map key type; use map[string]T with a supported primitive T')
	}
}

// decode_any_map decodes a TOML table value.
fn decode_any_map[K, V](any toml.Any, setting string, is_secret bool) !map[K]V {
	$if K is string {
		$if V is string {
			values := if any is map[string]toml.Any {
				any
			} else {
				return ConfigError{
					setting:  setting
					source:   'file'
					expected: 'map'
					reason:   'expected a table value'
				}
			}
			mut out := map[K]V{}
			for k, item in values {
				if item is []toml.Any || item is map[string]toml.Any {
					return ConfigError{
						setting:  map_entry_setting(setting, k, is_secret)
						source:   'file'
						expected: 'string'
						reason:   'expected a string value'
					}
				}
				out[K(k)] = V(any_to_scalar_string(item))
			}
			return out
		} $else $if V is bool {
			values := if any is map[string]toml.Any {
				any
			} else {
				return ConfigError{
					setting:  setting
					source:   'file'
					expected: 'map'
					reason:   'expected a table value'
				}
			}
			mut out := map[K]V{}
			for k, item in values {
				if item is []toml.Any || item is map[string]toml.Any {
					return ConfigError{
						setting:  map_entry_setting(setting, k, is_secret)
						source:   'file'
						expected: 'bool'
						reason:   'expected a boolean value'
					}
				}
				out[K(k)] = V(parse_config_bool(map_entry_setting(setting, k, is_secret), any_to_scalar_string(item),
					'file', is_secret)!)
			}
			return out
		} $else $if V is int {
			values := if any is map[string]toml.Any {
				any
			} else {
				return ConfigError{
					setting:  setting
					source:   'file'
					expected: 'map'
					reason:   'expected a table value'
				}
			}
			mut out := map[K]V{}
			for k, item in values {
				if item is []toml.Any || item is map[string]toml.Any {
					return ConfigError{
						setting:  map_entry_setting(setting, k, is_secret)
						source:   'file'
						expected: 'int'
						reason:   'expected an integer value'
					}
				}
				out[K(k)] = V(parse_config_int(map_entry_setting(setting, k, is_secret), any_to_scalar_string(item),
					'file', is_secret)!)
			}
			return out
		} $else $if V is u16 {
			values := if any is map[string]toml.Any {
				any
			} else {
				return ConfigError{
					setting:  setting
					source:   'file'
					expected: 'map'
					reason:   'expected a table value'
				}
			}
			mut out := map[K]V{}
			for k, item in values {
				if item is []toml.Any || item is map[string]toml.Any {
					return ConfigError{
						setting:  map_entry_setting(setting, k, is_secret)
						source:   'file'
						expected: 'u16'
						reason:   'expected an integer value'
					}
				}
				out[K(k)] = V(parse_config_u16(map_entry_setting(setting, k, is_secret), any_to_scalar_string(item),
					'file', is_secret)!)
			}
			return out
		} $else $if V is i64 {
			values := if any is map[string]toml.Any {
				any
			} else {
				return ConfigError{
					setting:  setting
					source:   'file'
					expected: 'map'
					reason:   'expected a table value'
				}
			}
			mut out := map[K]V{}
			for k, item in values {
				if item is []toml.Any || item is map[string]toml.Any {
					return ConfigError{
						setting:  map_entry_setting(setting, k, is_secret)
						source:   'file'
						expected: 'i64'
						reason:   'expected an integer value'
					}
				}
				out[K(k)] = V(parse_config_i64(map_entry_setting(setting, k, is_secret), any_to_scalar_string(item),
					'file', is_secret)!)
			}
			return out
		} $else $if V is f64 {
			values := if any is map[string]toml.Any {
				any
			} else {
				return ConfigError{
					setting:  setting
					source:   'file'
					expected: 'map'
					reason:   'expected a table value'
				}
			}
			mut out := map[K]V{}
			for k, item in values {
				if item is []toml.Any || item is map[string]toml.Any {
					return ConfigError{
						setting:  map_entry_setting(setting, k, is_secret)
						source:   'file'
						expected: 'f64'
						reason:   'expected a float value'
					}
				}
				out[K(k)] = V(parse_config_f64(map_entry_setting(setting, k, is_secret), any_to_scalar_string(item),
					'file', is_secret)!)
			}
			return out
		} $else {
			$compile_error('config: unsupported map value type; use maps of string, int, bool, u16, i64 or f64')
		}
	} $else {
		$compile_error('config: unsupported map key type; use map[string]T with a supported primitive T')
	}
}

// parse_cli_args folds raw args into a key->value map.
fn parse_cli_args(args []string) map[string]string {
	mut out := map[string]string{}
	mut i := 0
	for i < args.len {
		a := args[i]
		if a.starts_with('--') {
			if eq := a.index('=') {
				out[a[..eq].replace('_', '-')] = a[eq + 1..]
			} else if i + 1 < args.len && !args[i + 1].starts_with('--') {
				out[a.replace('_', '-')] = args[i + 1]
				i++
			} else {
				out[a.replace('_', '-')] = 'true'
			}
		}
		i++
	}
	return out
}

// attr_arg returns the argument of a `name: 'arg'` field attribute, if any.
fn attr_arg(attrs []string, name string) ?string {
	for attr in attrs {
		if idx := attr.index(':') {
			if attr[..idx].trim_space() == name {
				return attr[idx + 1..].trim_space().trim('\'"')
			}
		}
	}
	return none
}

// has_attr reports whether a bare or valued attribute is present.
fn has_attr(attrs []string, name string) bool {
	for attr in attrs {
		if idx := attr.index(':') {
			if attr[..idx].trim_space() == name {
				return true
			}
		} else if attr.trim_space() == name {
			return true
		}
	}
	return false
}

// has_secret_value reports whether T (transitively) marks any field @[secret].
// TOML parse errors echo source lines, so load() redacts them when the
// target config can hold secrets.
fn has_secret_value[T](v T) bool {
	$if T is $struct {
		$for field in T.fields {
			if has_attr(field.attrs, 'secret') {
				return true
			}
			$if field.is_struct {
				if has_secret_value(v.$(field.name)) {
					return true
				}
			}
		}
	}
	return false
}

// map_entry_setting names a map entry diagnostic. Secret maps omit the
// caller-supplied key so sensitive input never appears in the setting path.
fn map_entry_setting(setting string, k string, is_secret bool) string {
	if is_secret {
		return setting
	}
	return '${setting}.${k}'
}

// int_error builds the ConfigError for an invalid integer.
fn int_error(setting string, source string, raw string, expected string, is_secret bool) IError {
	reason := if is_secret {
		'not a valid ${expected} (value redacted)'
	} else {
		'not a valid ${expected}: `${raw}`'
	}
	return ConfigError{
		setting:  setting
		source:   source
		expected: expected
		reason:   reason
	}
}

// parse_config_int converts a raw value strictly (no silent 0 on garbage).
fn parse_config_int(setting string, raw string, source string, is_secret bool) !int {
	mut s := raw
	if s.starts_with('-') {
		s = s[1..]
	}
	if s == '' {
		return int_error(setting, source, raw, 'int', is_secret)
	}
	for c in s.bytes() {
		if c < `0` || c > `9` {
			return int_error(setting, source, raw, 'int', is_secret)
		}
	}
	return strconv.atoi(raw) or {
		return int_error(setting, source, raw, 'int', is_secret)
	}
}

// parse_config_u16 converts a raw value strictly to 0..65535.
fn parse_config_u16(setting string, raw string, source string, is_secret bool) !u16 {
	if raw == '' {
		return int_error(setting, source, raw, 'u16', is_secret)
	}
	for c in raw.bytes() {
		if c < `0` || c > `9` {
			return int_error(setting, source, raw, 'u16', is_secret)
		}
	}
	n := strconv.parse_uint(raw, 10, 16) or {
		return int_error(setting, source, raw, 'u16', is_secret)
	}
	return u16(n)
}

// parse_config_i64 converts a raw value strictly to a 64-bit integer.
fn parse_config_i64(setting string, raw string, source string, is_secret bool) !i64 {
	mut s := raw
	if s.starts_with('-') {
		s = s[1..]
	} else if s.starts_with('+') {
		return int_error(setting, source, raw, 'i64', is_secret)
	}
	if s == '' {
		return int_error(setting, source, raw, 'i64', is_secret)
	}
	for c in s.bytes() {
		if c < `0` || c > `9` {
			return int_error(setting, source, raw, 'i64', is_secret)
		}
	}
	return strconv.atoi64(raw) or {
		return int_error(setting, source, raw, 'i64', is_secret)
	}
}

// parse_config_f64 converts a raw value strictly to a 64-bit float.
fn parse_config_f64(setting string, raw string, source string, is_secret bool) !f64 {
	if raw == '' {
		return int_error(setting, source, raw, 'f64', is_secret)
	}
	if raw.contains_u8(`_`) || raw.contains_u8(` `) {
		return int_error(setting, source, raw, 'f64', is_secret)
	}
	return strconv.atof64(raw) or {
		return int_error(setting, source, raw, 'f64', is_secret)
	}
}

// parse_config_bool converts true/1/yes and false/0/no (case-insensitive).
fn parse_config_bool(setting string, raw string, source string, is_secret bool) !bool {
	match raw.to_lower() {
		'true', '1', 'yes' {
			return true
		}
		'false', '0', 'no' {
			return false
		}
		else {
			reason := if is_secret {
				'not a valid bool (value redacted)'
			} else {
				'not a valid bool: `${raw}`'
			}
			return ConfigError{
				setting:  setting
				source:   source
				expected: 'bool'
				reason:   reason
			}
		}
	}
}
