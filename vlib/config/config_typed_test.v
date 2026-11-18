module config

import os

struct PrimitiveConfig {
	count u16    @[default: '42']
	big   i64    @[default: '-9999999999']
	ratio f64    @[default: '1.5']
	name  string @[default: 'default-name']
}

fn test_config_new_primitives_defaults() {
	cfg := load[PrimitiveConfig](env_prefix: 'VLIB_TEST_PRIM_UNSET_')!
	assert cfg.count == 42
	assert cfg.big == -9999999999
	assert cfg.ratio == 1.5
	assert cfg.name == 'default-name'
}

fn test_config_new_primitives_cli() {
	cfg := load[PrimitiveConfig](
		cli:        ['--count=100', '--big=9223372036854775807', '--ratio=2.5', '--name=cli']
		env_prefix: 'VLIB_TEST_PRIM_UNSET_'
	)!
	assert cfg.count == 100
	assert cfg.big == 9223372036854775807
	assert cfg.ratio == 2.5
	assert cfg.name == 'cli'
}

fn test_config_new_primitives_env() {
	os.setenv('VLIB_TEST_P2_COUNT', '200', true)
	os.setenv('VLIB_TEST_P2_BIG', '-1234567890123', true)
	os.setenv('VLIB_TEST_P2_RATIO', '3.25', true)
	defer {
		os.unsetenv('VLIB_TEST_P2_COUNT')
		os.unsetenv('VLIB_TEST_P2_BIG')
		os.unsetenv('VLIB_TEST_P2_RATIO')
	}
	cfg := load[PrimitiveConfig](env_prefix: 'VLIB_TEST_P2_')!
	assert cfg.count == 200
	assert cfg.big == -1234567890123
	assert cfg.ratio == 3.25
}

fn test_config_new_primitives_file() {
	path := os.join_path(os.temp_dir(), 'v_config_prim.toml')
	os.write_file(path, 'count = 300\nbig = 1234567890123\nratio = 4.5\nname = "file"\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	cfg := load[PrimitiveConfig](files: [path])!
	assert cfg.count == 300
	assert cfg.big == 1234567890123
	assert cfg.ratio == 4.5
	assert cfg.name == 'file'
}

fn test_config_u16_rejects_out_of_range() {
	load[PrimitiveConfig](cli: ['--count=70000'], env_prefix: 'VLIB_TEST_PRIM_UNSET_') or {
		assert err.msg().contains('`count`')
		assert err.msg().contains('[cli]')
		assert err.msg().contains('expected u16')
		return
	}
	assert false, 'u16 overflow must error'
	load[PrimitiveConfig](cli: ['--count=-1'], env_prefix: 'VLIB_TEST_PRIM_UNSET_') or {
		assert err.msg().contains('expected u16')
		return
	}
	assert false, 'negative u16 must error'
}

fn test_config_i64_rejects_overflow() {
	load[PrimitiveConfig](
		cli:        ['--big=9223372036854775808']
		env_prefix: 'VLIB_TEST_PRIM_UNSET_'
	) or {
		assert err.msg().contains('expected i64')
		return
	}
	assert false, 'i64 overflow must error'
}

fn test_config_f64_rejects_garbage() {
	load[PrimitiveConfig](cli: ['--ratio=abc'], env_prefix: 'VLIB_TEST_PRIM_UNSET_') or {
		assert err.msg().contains('expected f64')
		return
	}
	assert false, 'bad f64 must error'
}

struct DbConfig {
	host string @[default: 'localhost']
	port int    @[default: '5432']
}

struct AppConfig {
	db    DbConfig
	debug bool   @[default: 'false']
	token string @[required]
}

fn test_config_nested_struct_file() {
	path := os.join_path(os.temp_dir(), 'v_config_nested.toml')
	os.write_file(path, 'debug = true\ntoken = "filetok"\n[db]\nhost = "file.db"\nport = 1111\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	cfg := load[AppConfig](files: [path])!
	assert cfg.db.host == 'file.db'
	assert cfg.db.port == 1111
	assert cfg.debug == true
	assert cfg.token == 'filetok'
}

fn test_config_nested_struct_cli_beats_file() {
	path := os.join_path(os.temp_dir(), 'v_config_nested2.toml')
	os.write_file(path, 'token = "filetok"\n[db]\nhost = "file.db"\nport = 1111\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	cfg := load[AppConfig](
		cli:   ['--db-host=cli.db', '--db-port=2222', '--token=clitok']
		files: [path]
	)!
	assert cfg.db.host == 'cli.db', 'cli must beat file for nested fields'
	assert cfg.db.port == 2222
	assert cfg.token == 'clitok'
}

fn test_config_nested_struct_env() {
	os.setenv('VLIB_TEST_N3_DB_HOST', 'env.db', true)
	os.setenv('VLIB_TEST_N3_DB_PORT', '3333', true)
	os.setenv('VLIB_TEST_N3_TOKEN', 'envtok', true)
	defer {
		os.unsetenv('VLIB_TEST_N3_DB_HOST')
		os.unsetenv('VLIB_TEST_N3_DB_PORT')
		os.unsetenv('VLIB_TEST_N3_TOKEN')
	}
	cfg := load[AppConfig](env_prefix: 'VLIB_TEST_N3_')!
	assert cfg.db.host == 'env.db'
	assert cfg.db.port == 3333
	assert cfg.token == 'envtok'
}

fn test_config_nested_struct_dot_cli() {
	cfg := load[AppConfig](cli: ['--db.host=dot.db', '--db.port=4444', '--token=t'])!
	assert cfg.db.host == 'dot.db'
	assert cfg.db.port == 4444
}

struct CollectionConfig {
	tags    []string       @[default: 'a,b']
	ports   []int          @[default: '80,443']
	flags   []bool         @[default: 'true,false']
	limits  map[string]int @[default: 'cpu=2,mem=4']
	weights map[string]f64 @[default: 'x=1.5']
}

fn test_config_collections_defaults() {
	cfg := load[CollectionConfig](env_prefix: 'VLIB_TEST_COLL_UNSET_')!
	assert cfg.tags == ['a', 'b']
	assert cfg.ports == [80, 443]
	assert cfg.flags == [true, false]
	assert cfg.limits == {
		'cpu': 2
		'mem': 4
	}
	assert cfg.weights == {
		'x': 1.5
	}
}

fn test_config_collections_cli() {
	cfg := load[CollectionConfig](
		cli:        ['--tags=x,y,z', '--ports=1,2', '--flags=yes,no', '--limits=a=10', '--weights=k=2.5']
		env_prefix: 'VLIB_TEST_COLL_UNSET_'
	)!
	assert cfg.tags == ['x', 'y', 'z']
	assert cfg.ports == [1, 2]
	assert cfg.flags == [true, false]
	assert cfg.limits == {
		'a': 10
	}
	assert cfg.weights == {
		'k': 2.5
	}
}

fn test_config_collections_file() {
	path := os.join_path(os.temp_dir(), 'v_config_coll.toml')
	os.write_file(path, 'tags = ["f1", "f2"]\nports = [9, 8]\nflags = [true]\n[limits]\na = 5\n[weights]\nw = 0.5\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	cfg := load[CollectionConfig](files: [path])!
	assert cfg.tags == ['f1', 'f2']
	assert cfg.ports == [9, 8]
	assert cfg.flags == [true]
	assert cfg.limits == {
		'a': 5
	}
	assert cfg.weights == {
		'w': 0.5
	}
}

fn test_config_collections_env_beats_file() {
	path := os.join_path(os.temp_dir(), 'v_config_coll2.toml')
	os.write_file(path, 'tags = ["file"]\nports = [1]\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	os.setenv('VLIB_TEST_C3_TAGS', 'env1,env2', true)
	defer {
		os.unsetenv('VLIB_TEST_C3_TAGS')
	}
	cfg := load[CollectionConfig](env_prefix: 'VLIB_TEST_C3_', files: [path])!
	assert cfg.tags == ['env1', 'env2'], 'env must beat file for arrays'
	assert cfg.ports == [1], 'file must beat default for arrays'
}

struct NativeDefaultConfig {
	port         int    = 8080
	host         string = 'native-host'
	mixed        int    = 9090 @[default: '1234']
	only_default int           @[default: '5678']
	token        string        @[default: 'tok']
}

fn test_config_native_initializer_wins_over_default() {
	cfg := load[NativeDefaultConfig](env_prefix: 'VLIB_TEST_NATIVE_UNSET_')!
	assert cfg.port == 8080, 'native initializer must be kept'
	assert cfg.host == 'native-host'
	assert cfg.mixed == 9090, 'native initializer must win over @[default]'
	assert cfg.only_default == 5678, '@[default] must apply without native'
}

fn test_config_source_overrides_native() {
	cfg := load[NativeDefaultConfig](
		cli:        ['--port=1', '--host=cli']
		env_prefix: 'VLIB_TEST_NATIVE_UNSET_'
	)!
	assert cfg.port == 1, 'cli must override native initializer'
	assert cfg.host == 'cli'
}

struct SecretConfig {
	token    string @[required; secret]
	password string @[default: 'hunter2'; secret]
	port     int    @[default: '8080']
}

fn test_config_secret_redacted_representation() {
	cfg := load[SecretConfig](cli: ['--token=realtoken'], env_prefix: 'VLIB_TEST_SEC_UNSET_')!
	out := redacted(cfg)
	assert out.contains("'***'"), 'secrets must be masked'
	assert !out.contains('realtoken'), 'real secret must not appear'
	assert !out.contains('hunter2'), 'default secret must not appear'
	assert out.contains('8080'), 'non-secret values stay visible'
}

struct SecretIntConfig {
	pin  int    @[secret]
	name string @[default: 'n']
}

fn test_config_secret_parse_error_redacted() {
	load[SecretIntConfig](cli: ['--pin=notanint'], env_prefix: 'VLIB_TEST_SEC2_UNSET_') or {
		assert !err.msg().contains('notanint'), 'secret raw value must not leak: ${err.msg()}'
		assert err.msg().contains('redacted'), 'secret errors must note redaction'
		assert err.msg().contains('`pin`'), 'setting must still be named'
		return
	}
	assert false, 'bad secret int must error'
}

struct PlainIntConfig {
	count int @[default: '5']
}

fn test_config_nonsecret_error_includes_value() {
	load[PlainIntConfig](cli: ['--count=badval'], env_prefix: 'VLIB_TEST_PLAIN_UNSET_') or {
		assert err.msg().contains('badval'), 'non-secret errors may include the value'
		assert err.msg().contains('`count`')
		assert err.msg().contains('[cli]')
		assert err.msg().contains('expected int')
		return
	}
	assert false, 'bad int must error'
}

fn test_config_structured_diagnostics_shape() {
	load[PlainIntConfig](cli: ['--count=zzz'], env_prefix: 'VLIB_TEST_DIAG_') or {
		msg := err.msg()
		assert msg.contains('config:'), msg
		assert msg.contains('`count`'), msg
		assert msg.contains('[cli]'), msg
		assert msg.contains('expected int'), msg
		return
	}
	assert false, 'must error with structured diagnostic'
}

fn test_config_missing_value_errors_never_silent() {
	load[PlainIntConfig](env_prefix: 'VLIB_TEST_MISS_UNSET_')!
	// has a default so it loads; now a struct without any default must error
	load[SecretIntConfig](env_prefix: 'VLIB_TEST_MISS2_UNSET_') or {
		assert err.msg().contains('[missing]'), err.msg()
		return
	}
	assert false, 'missing required-ish value must error'
}

struct PrecConfig {
	host string @[default: 'defaulthost']
	port int    @[default: '1000']
}

fn test_config_precedence_every_layer() {
	path := os.join_path(os.temp_dir(), 'v_config_prec_all.toml')
	os.write_file(path, 'host = "filehost"\nport = 2000\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	// file beats defaults
	f := load[PrecConfig](files: [path], env_prefix: 'VLIB_TEST_PREC_UNSET_')!
	assert f.host == 'filehost'
	assert f.port == 2000
	// env beats file
	os.setenv('VLIB_TEST_PREC_HOST', 'envhost', true)
	defer {
		os.unsetenv('VLIB_TEST_PREC_HOST')
	}
	e := load[PrecConfig](files: [path], env_prefix: 'VLIB_TEST_PREC_')!
	assert e.host == 'envhost', 'env must beat file'
	assert e.port == 2000, 'file must beat default when env is silent'
	// cli beats env
	c := load[PrecConfig](cli: ['--host=clihost'], files: [path], env_prefix: 'VLIB_TEST_PREC_')!
	assert c.host == 'clihost', 'cli must beat env'
	assert c.port == 2000
	// cli beats everything for port too
	c2 := load[PrecConfig](cli: ['--port=3000'], files: [path], env_prefix: 'VLIB_TEST_PREC_')!
	assert c2.port == 3000, 'cli must beat file'
	assert c2.host == 'envhost', 'env still beats file for other keys'
}
