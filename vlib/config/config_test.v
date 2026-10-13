module config

import os

struct ServerConfig {
	host  string @[env: 'VLIB_TEST_HOST'; default: 'localhost']
	port  int    @[default: '8080']
	debug bool   @[default: 'false']
	token string @[required]
}

fn test_config_defaults_and_required() {
	defaults := load[ServerConfig](cli: ['--token=provided'], env_prefix: 'VLIB_TEST_UNSET_')!
	assert defaults.host == 'localhost'
	assert defaults.port == 8080
	assert !defaults.debug

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

fn test_config_invalid_int() {
	load[ServerConfig](cli: ['--port=abc', '--token=tok'], env_prefix: 'VLIB_TEST_BAD_') or {
		assert err.msg() == 'config: `port` is not a valid int: `abc`'
		return
	}
	assert false, 'bad int must error'
}

fn test_config_invalid_bool() {
	load[ServerConfig](cli: ['--debug=maybe', '--token=tok'], env_prefix: 'VLIB_TEST_BAD_') or {
		assert err.msg() == 'config: `debug` is not a valid bool: `maybe`'
		return
	}
	assert false, 'bad bool must error'
}

fn test_config_missing_file() {
	load[ServerConfig](files: ['/nonexistent-dir-xyz/app.toml']) or {
		assert err.msg().contains('cannot read')
		return
	}
	assert false, 'missing file must error'
}

fn test_config_integer_boundaries_and_overflow() {
	for raw in ['2147483647', '-2147483648', '0', '-0', '00010'] {
		assert parse_config_int('port', raw)! == raw.int()
	}
	for raw in ['2147483648', '-2147483649', '4294967296', '18446744073709551616',
		'999999999999999999999999999999', '-', '+1', '1_000', ' 1'] {
		if _ := parse_config_int('port', raw) {
			assert false, 'must reject ${raw}'
		} else {
			assert err.msg() == 'config: `port` is not a valid int: `${raw}`'
		}
	}
}

fn test_config_cli_alias_empty_value_and_bare_boolean() {
	cfg := load[ServerConfig](
		cli:        ['--host=', '--token=tok', '--debug']
		env_prefix: 'VLIB_TEST_EMPTY_'
	)!
	assert cfg.host == ''
	assert cfg.debug
}

struct AliasConfig {
	host_name string @[default: 'localhost']
}

fn test_config_cli_aliases_use_last_argument() {
	cfg := load[AliasConfig](
		cli:        ['--host_name=first', '--host-name', 'second']
		env_prefix: 'VLIB_TEST_ALIAS_'
	)!
	assert cfg.host_name == 'second'
	reverse := load[AliasConfig](
		cli:        ['--host-name=first', '--host_name=second']
		env_prefix: 'VLIB_TEST_ALIAS_'
	)!
	assert reverse.host_name == 'second'
}

fn test_unsupported_field_type_is_a_compile_error() ! {
	fixture_root := os.join_path(os.vtmp_dir(), 'config unsupported ${os.getpid()}')
	os.mkdir_all(fixture_root)!
	defer {
		os.rmdir_all(fixture_root) or {}
	}
	fixture := os.join_path(fixture_root, 'unsupported.v')
	os.write_file(fixture, "import config\nstruct UnsupportedConfig { value f64 @[default: '1.0'] }\nfn main() { cfg := config.load[UnsupportedConfig](config.LoadOptions{})! println(cfg) }\n")!
	module_path := os.join_path(@VMODROOT, 'vlib') + '|@vlib|@vmodules'
	result := os.exec([@VEXE, '-enable-globals', '-path', module_path, '-o',
		os.join_path(fixture_root, 'unsupported'), fixture])
	assert result.exit_code != 0, result.output
	assert result.output.contains('config: unsupported field type; use string, int or bool'), result.output
}
