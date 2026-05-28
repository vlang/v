module yaml

import os

enum Role {
	worker
	manager
}

struct Address {
	city string
	zip  int
}

struct AppConfig {
	name    string
	role    Role @[json: 'role']
	enabled bool
	ports   []int
	address Address
	notes   string
}

fn test_parse_doc_queries_and_block_scalars() ! {
	doc := parse_text('title: Example
config: { enabled: true, retries: 3 }
servers:
  - host: api.local
    ports: [80, 443]
  - host: jobs.local
quoted:
  "a.b": 7
notes: |
  first line
  second line
folded: >
  hello
  world
')!

	assert doc.value('title').string() == 'Example'
	assert doc.value('config.enabled').bool()
	assert doc.value('config.retries').int() == 3
	assert doc.value('servers[0].host').string() == 'api.local'
	assert doc.value('servers[0].ports[1]').int() == 443
	assert doc.value('quoted."a.b"').int() == 7
	assert doc.value('notes').string() == 'first line\nsecond line\n'
	assert doc.value('folded').string() == 'hello world\n'
}

fn test_generic_encode_decode_with_json_attrs() ! {
	config := AppConfig{
		name:    'worker'
		role:    .manager
		enabled: true
		ports:   [8080, 9090]
		address: Address{
			city: 'Springwood'
			zip:  1428
		}
		notes:   'line one\nline two'
	}

	encoded := encode(config)
	assert encoded.contains('"role": "manager"')
	assert encoded.contains('"address":')
	assert encoded.contains('- 8080')

	decoded := decode[AppConfig](encoded)!
	assert decoded == config
}

fn test_file_helpers() ! {
	path := os.join_path(os.vtmp_dir(), 'yaml_test_${os.getpid()}.yml')
	defer {
		os.rm(path) or {}
	}
	config := AppConfig{
		name:    'batch11'
		role:    .worker
		enabled: false
		ports:   [7000]
		address: Address{
			city: 'Moscow'
			zip:  101000
		}
		notes:   'plain'
	}

	encode_file(path, config)!
	decoded := decode_file[AppConfig](path)!
	assert decoded == config

	doc := parse_file(path)!
	assert doc.value('name').string() == 'batch11'
	assert doc.value('role').string() == 'worker'
	assert doc.value('address.city').string() == 'Moscow'
}
