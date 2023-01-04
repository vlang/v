import os
import toml
import toml.to

const toml_text = os.read_file(
	os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
	'.toml') or { panic(err) }

fn test_toml() {
	// File containing the complete text from the example in the official TOML project README.md:
	// https://github.com/toml-lang/toml/blob/3b11f6921da7b6f5db37af039aa021fee450c091/README.md#Example
	toml_doc := toml.parse_text(toml_text) or { panic(err) }
	toml_json := to.json(toml_doc)

	// NOTE Kept for easier debugging:
	// dump(toml_doc.ast)
	// println(toml_json)
	// assert false

	assert toml_json == os.read_file(
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.out') or { panic(err) }

	title := toml_doc.value('title')
	assert title == toml.Any('TOML Example')
	assert title as string == 'TOML Example'

	database := toml_doc.value('database') as map[string]toml.Any
	db_serv := database['server'] or {
		panic('could not access "server" index in "database" variable')
	}
	assert db_serv as string == '192.168.1.1'

	assert toml_doc.value('owner.name') as string == 'Tom Preston-Werner'

	assert toml_doc.value('database.server') as string == '192.168.1.1'

	database_ports := toml_doc.value('database.ports') as []toml.Any
	assert database_ports[0] as i64 == 8000
	assert database_ports[1] as i64 == 8001
	assert database_ports[2] as i64 == 8002
	assert database_ports[0].int() == 8000
	assert database_ports[1].int() == 8001
	assert database_ports[2].int() == 8002

	assert toml_doc.value('database.connection_max') as i64 == 5000
	assert toml_doc.value('database.enabled') as bool == true

	assert toml_doc.value('servers.alpha.ip').string() == '10.0.0.1'
	assert toml_doc.value('servers.alpha.dc').string() == 'eqdc10'

	assert toml_doc.value('servers.beta.ip').string() == '10.0.0.2'
	assert toml_doc.value('servers.beta.dc').string() == 'eqdc10'

	clients_data := (toml_doc.value('clients.data') as []toml.Any)
	// dump(clients_data)
	// assert false
	gamma_delta_array := clients_data[0] as []toml.Any
	digits_array := clients_data[1] as []toml.Any
	assert gamma_delta_array[0].string() == 'gamma'
	assert gamma_delta_array[1].string() == 'delta'
	assert digits_array[0].int() == 1
	assert digits_array[1].int() == 2

	clients_hosts := (toml_doc.value('clients.hosts') as []toml.Any).as_strings()
	assert clients_hosts[0] == 'alpha'
	assert clients_hosts[1] == 'omega'
}

fn test_toml_file() {
	out_path := os.join_path(os.vtmp_dir(), 'v', 'toml_tests')
	test_file := os.join_path(out_path, 'toml_example.toml')
	os.mkdir_all(out_path) or { assert false }
	defer {
		os.rmdir_all(out_path) or {}
	}
	os.write_file(test_file, toml_text) or { assert false }
	toml_doc := toml.parse_file(test_file) or { panic(err) }

	toml_json := to.json(toml_doc)

	// NOTE Kept for easier debugging:
	// dump(toml_doc.ast)
	// println(toml_json)
	// assert false

	assert toml_json == os.read_file(
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.out') or { panic(err) }
}

fn test_toml_parse_text() {
	toml_doc := toml.parse_text(toml_text) or { panic(err) }
	toml_json := to.json(toml_doc)
	assert toml_json == os.read_file(
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.out') or { panic(err) }
}

fn test_toml_parse() {
	toml_doc := toml.parse_text(toml_text) or { panic(err) }
	toml_json := to.json(toml_doc)
	assert toml_json == os.read_file(
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.out') or { panic(err) }
}
