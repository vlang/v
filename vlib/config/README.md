# config

`config.load[T]` fills a struct with string, int, and bool fields from CLI arguments,
environment variables, TOML files, and field defaults, in that order.

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

CLI accepts `--field=value`, `--field value`, and bare `--field` as `true`. Underscores in
field names may be written as dashes; the last CLI value wins across both spellings.
Environment keys default to the prefix plus uppercase
field name; `@[env: 'NAME']` uses that exact key instead. The first TOML file containing a
matching top-level key wins. Unreadable or invalid files return an error.

`@[default: 'value']` supplies a fallback. Missing fields return an error; `@[required]`
provides a required-key diagnostic. Empty strings are provided values. Decimal integers must
fit V's signed 32-bit `int`; invalid values and overflow return errors. Booleans accept
`true`, `1`, `yes`, `false`, `0`, and `no`, ignoring case. Other field types are unsupported.
