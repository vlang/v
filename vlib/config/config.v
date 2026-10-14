module config

import os
import toml
import strconv

// LoadOptions selects where configuration values come from, in precedence
// order: cli, then env, then files (first hit wins), then @[default].
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

// load fills a struct from the configured sources. Each field takes its
// value from the first source that provides it:
//
//  1. cli: `--field` or `--field-with-dashes`, as `--k=v` or `--k v`
//  2. env: `@[env: 'NAME']`, else `env_prefix + UPPER(field)`
//  3. files: the TOML top-level key matching the field name
//  4. `@[default: '...']`
//
// A field with no value and no default is an error, as is a field marked
// `@[required]`. Supported field types are string, int and bool; anything
// else is a compile error. Example:
//
// ```v
// struct ServerConfig {
// 	host  string @[env: 'HOST'; default: 'localhost']
// 	port  int    @[default: '8080']
// 	debug bool   @[default: 'false']
// 	token string @[required]
// }
//
// cfg := config.load[ServerConfig](cli: os.args[1..], env_prefix: 'APP_',
// 	files: ['app.toml', '/etc/app.toml'])!
// ```
pub fn load[T](opts LoadOptions) !T {
	cli := parse_cli_args(opts.cli)
	mut docs := []toml.Doc{}
	for path in opts.files {
		docs << toml.parse_file(path) or {
			return error('config: cannot read `${path}`: ${err.msg()}')
		}
	}
	mut cfg := T{}
	$for field in T.fields {
		raw := resolve_raw(field.name, field.attrs, cli, opts.env_prefix, docs)!
		$if field.typ is string {
			cfg.$(field.name) = raw
		} $else $if field.typ is int {
			cfg.$(field.name) = parse_config_int(field.name, raw)!
		} $else $if field.typ is bool {
			cfg.$(field.name) = parse_config_bool(field.name, raw)!
		} $else {
			check_field_type(cfg.$(field.name))
		}
	}
	return cfg
}

// resolve_raw finds the winning raw string for one field, or errors.
fn resolve_raw(name string, attrs []string, cli map[string]string, env_prefix string, docs []toml.Doc) !string {
	if v := cli['--' + name.replace('_', '-')] {
		return v
	}
	env_name := attr_arg(attrs, 'env') or { env_prefix + name.to_upper() }
	if v := os.getenv_opt(env_name) {
		return v
	}
	for doc in docs {
		if v := doc.value_opt(name) {
			// Any.string() debug-formats non-strings, so render scalars
			// directly; complex values fall back to their TOML encoding.
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
	}
	if d := attr_arg(attrs, 'default') {
		return d
	}
	if has_attr(attrs, 'required') {
		return error('config: required key `${name}` was not provided')
	}
	return error('config: no value for `${name}` and no default')
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

// parse_config_int converts a raw value strictly (no silent 0 on garbage).
fn parse_config_int(field string, raw string) !int {
	mut s := raw
	if s.starts_with('-') {
		s = s[1..]
	}
	if s == '' {
		return error('config: `${field}` is not a valid int: `${raw}`')
	}
	for c in s.bytes() {
		if c < `0` || c > `9` {
			return error('config: `${field}` is not a valid int: `${raw}`')
		}
	}
	return strconv.atoi(raw) or {
		return error('config: `${field}` is not a valid int: `${raw}`')
	}
}

// parse_config_bool converts true/1/yes and false/0/no (case-insensitive).
fn parse_config_bool(field string, raw string) !bool {
	match raw.to_lower() {
		'true', '1', 'yes' { return true }
		'false', '0', 'no' { return false }
		else { return error('config: `${field}` is not a valid bool: `${raw}`') }
	}
}

fn check_field_type[T](value T) {
	_ = value
	$if T is string || T is int || T is bool {
	} $else {
		$compile_error('config: unsupported field type; use string, int or bool')
	}
}
