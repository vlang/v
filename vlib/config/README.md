# config

`config.load[T]` fills a struct from CLI arguments, environment variables, TOML
files, field defaults, and native initializers, in that order. Supported field
types are `string`, `int`, `bool`, `u16`, `i64`, `f64`, nested structs, and
arrays/maps of those primitives (`[]string`, `map[string]int`, ...). Anything
else is a compile error.

```v
import config

struct ServerConfig {
	host  string @[default: 'localhost']
	port  int    @[default: '8080']
	debug bool   @[default: 'false']
}

fn main() {
	cfg := config.load[ServerConfig](cli: ['--port=9090'], env_prefix: 'APP_')!
	println(cfg.port) // 9090
}
```

CLI accepts `--field=value`, `--field value`, and bare `--field` as `true`.
Underscores in field names may be written as dashes; the last CLI value wins
across both spellings. Nested fields use `--parent-child` (or
`--parent.child`), with underscores and dashes interchangeable in each
segment. Arrays use comma-separated values (`--tags=a,b`); maps use
comma-separated pairs (`--limits=cpu=2,mem=4G`). Environment keys default to
the prefix plus uppercase field name; `@[env: 'NAME']` uses that exact key
instead. Nested fields use `PREFIX_PARENT_CHILD` (e.g. `APP_DB_HOST`), with the
same comma-separated syntax for collections. The first TOML file containing a
matching key wins; nested structs read TOML tables via dotted paths (e.g.
`db.host`), and a non-table TOML value for a nested struct is an error;
collections read native TOML arrays and tables. Unreadable or
invalid files return an error.

Precedence is `defaults < file < env < cli`: a source overrides the merged
result only when it provides the key. Empty strings are provided values.

`@[default: 'value']` supplies a fallback parsed with the same strict rules as
every other source. A native `= value` initializer wins over `@[default]` when
no source provides the key; declare `@[default]` for values that must be
visible to tooling, native initializers for plain code defaults. Missing fields
return an error; `@[required]` provides a required-key diagnostic. Values are
never silently substituted on parse failure.

`@[secret]` marks a sensitive field: its value is omitted from error output
(see `config.redacted(cfg)` for a masked debug representation, which renders
nested structs recursively) and never interpolated into diagnostics. For
configs containing secrets, TOML parse errors omit parser details and secret
map keys are omitted from setting paths.

Integers are strict decimal without `+`, spaces, or underscores: `int` must fit
signed 32-bit, `u16` fits `0..65535`, `i64` fits signed 64-bit. Floats parse as
`f64` without underscores or spaces. Booleans accept `true`, `1`, `yes`,
`false`, `0`, and `no`, ignoring case.

Failures return `config.ConfigError` with `setting` (dotted path, e.g.
`db.host`), `source` (`cli`, `env`, `file`, `default`, `missing`), `expected`
(the type), and `reason`. Values are loaded at runtime only; builds never read
the environment.
