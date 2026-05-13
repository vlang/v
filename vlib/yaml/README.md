## Description

`yaml` provides a pure-V YAML reader and writer for common configuration files.
It supports nested mappings, sequences, flow-style collections, block scalars,
tree access through `yaml.Any`, and generic struct encode/decode.

The generic `encode`/`decode` path delegates to the main `json` module so it
matches existing JSON field behavior, including `@[json: 'name']`.

## Usage

```v
import yaml

struct Config {
	name    string
	enabled bool
	ports   []int
}

const config_text = '
name: app
enabled: true
ports:
  - 8080
  - 9090
'

fn main() {
	doc := yaml.parse_text(config_text)!
	assert doc.value('ports[1]').int() == 9090

	config := yaml.decode[Config](config_text)!
	assert config.name == 'app'
	assert yaml.encode(config).contains('"name": "app"')
}
```

## File helpers

```v
import yaml

struct Config {
	name string
}

fn main() {
	config := yaml.decode_file[Config]('config.yml')!
	yaml.encode_file('config.out.yml', config)!
}
```
