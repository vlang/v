module config

import os

struct ServerConfig {
	host  string @[env: 'VLIB_TEST_HOST'; default: 'localhost']
	port  int    @[default: '8080']
	debug bool   @[default: 'false']
	token string @[required]
}

fn test_config_defaults_and_required() {
	load[ServerConfig](env_prefix: 'VLIB_TEST_UNSET_') or {
		assert err.msg() == 'config: required key `token` was not provided'
		return
	}
	assert false, 'missing required field must error'
}

fn test_config_env_and_types() {
	os.setenv('VLIB_TEST_HOST', 'env.example.com', true)
	os.setenv('VLIB_TEST_PORT', '9090', true)
	os.setenv('VLIB_TEST_DEBUG', 'yes', true)
	os.setenv('VLIB_TEST_TOKEN', 's3cret', true)
	defer {
		os.unsetenv('VLIB_TEST_HOST')
		os.unsetenv('VLIB_TEST_PORT')
		os.unsetenv('VLIB_TEST_DEBUG')
		os.unsetenv('VLIB_TEST_TOKEN')
	}
	cfg := load[ServerConfig](env_prefix: 'VLIB_TEST_')!
	assert cfg.host == 'env.example.com', 'explicit @[env] must win over default'
	assert cfg.port == 9090
	assert cfg.debug == true
	assert cfg.token == 's3cret'
}

fn test_config_cli_beats_env() {
	os.setenv('VLIB_TEST2_HOST', 'env.example.com', true)
	os.setenv('VLIB_TEST2_TOKEN', 'tok', true)
	defer {
		os.unsetenv('VLIB_TEST2_HOST')
		os.unsetenv('VLIB_TEST2_TOKEN')
	}
	cfg := load[ServerConfig](
		cli:        ['--host', 'cli.example.com', '--port=7070']
		env_prefix: 'VLIB_TEST2_'
	)!
	assert cfg.host == 'cli.example.com'
	assert cfg.port == 7070
	assert cfg.token == 'tok'
}

fn test_config_toml_file() {
	path := os.join_path(os.temp_dir(), 'v_config_test.toml')
	os.write_file(path, 'host = "file.example.com"\nport = 6060\ndebug = true\ntoken = "filetok"\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	cfg := load[ServerConfig](files: [path])!
	assert cfg.host == 'file.example.com'
	assert cfg.port == 6060
	assert cfg.debug == true
	assert cfg.token == 'filetok'
}

fn test_config_env_beats_file() {
	path := os.join_path(os.temp_dir(), 'v_config_prec.toml')
	os.write_file(path, 'host = "file.example.com"\nport = 6060\ntoken = "filetok"\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	os.setenv('VLIB_TEST5_HOST', 'env.example.com', true)
	os.setenv('VLIB_TEST5_TOKEN', 'envtok', true)
	defer {
		os.unsetenv('VLIB_TEST5_HOST')
		os.unsetenv('VLIB_TEST5_TOKEN')
	}
	// host has an explicit @[env], so it needs its own variable here
	os.setenv('VLIB_TEST_HOST', 'env.example.com', true)
	defer {
		os.unsetenv('VLIB_TEST_HOST')
	}
	cfg := load[ServerConfig](env_prefix: 'VLIB_TEST5_', files: [path])!
	assert cfg.host == 'env.example.com', 'env must beat files'
	assert cfg.port == 6060, 'files must beat defaults'
	assert cfg.token == 'envtok'
}

fn test_config_invalid_values_and_missing_file() {
	os.setenv('VLIB_TEST3_PORT', 'abc', true)
	os.setenv('VLIB_TEST3_TOKEN', 'tok', true)
	load[ServerConfig](env_prefix: 'VLIB_TEST3_') or {
		assert err.msg() == 'config: `port` is not a valid int: `abc`'
		return
	}
	assert false, 'bad int must error'
	os.setenv('VLIB_TEST4_DEBUG', 'maybe', true)
	os.setenv('VLIB_TEST4_TOKEN', 'tok', true)
	load[ServerConfig](env_prefix: 'VLIB_TEST4_') or {
		assert err.msg() == 'config: `debug` is not a valid bool: `maybe`'
		return
	}
	assert false, 'bad bool must error'
	load[ServerConfig](files: ['/nonexistent-dir-xyz/app.toml']) or {
		assert err.msg().contains('cannot read')
		return
	}
	assert false, 'missing file must error'
}
