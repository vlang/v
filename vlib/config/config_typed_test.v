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

struct SecretFileConfig {
	token string @[secret]
}

fn test_config_secret_toml_parse_error_redacted() {
	path := os.join_path(os.temp_dir(), 'v_config_secret_parse.toml')
	os.write_file(path, 'token = FAKE_SECRET_32494\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	load[SecretFileConfig](files: [path]) or {
		assert !err.msg().contains('FAKE_SECRET_32494'), 'parse error must not echo secret input: ${err.msg()}'
		assert err.msg().contains('redacted'), 'secret parse errors must note redaction: ${err.msg()}'
		return
	}
	assert false, 'invalid TOML must error'
}

fn test_config_nosecret_toml_parse_error_keeps_details() {
	path := os.join_path(os.temp_dir(), 'v_config_plain_parse.toml')
	os.write_file(path, 'count = FAKE_PLAINTEXT_32494\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	load[PlainIntConfig](files: [path]) or {
		assert err.msg().contains('cannot read'), err.msg()
		assert !err.msg().contains('redacted'), 'non-secret errors keep parser details: ${err.msg()}'
		return
	}
	assert false, 'invalid TOML must error'
}

struct SecretMapConfig {
	credentials map[string]int @[secret]
}

fn test_config_secret_map_cli_key_redacted() {
	load[SecretMapConfig](
		cli:        ['--credentials=FAKE_SECRET_MAP_KEY=invalid']
		env_prefix: 'VLIB_TEST_SECMAP_UNSET_'
	) or {
		msg := err.msg()
		assert !msg.contains('FAKE_SECRET_MAP_KEY'), 'secret map input must not leak: ${msg}'
		assert msg.contains('`credentials`'), 'setting must still be named: ${msg}'
		assert msg.contains('redacted'), 'secret errors must note redaction: ${msg}'
		return
	}
	assert false, 'bad secret map value must error'
}

fn test_config_secret_map_file_key_redacted() {
	path := os.join_path(os.temp_dir(), 'v_config_secret_map.toml')
	os.write_file(path, '[credentials]\nFAKE_FILE_MAP_KEY = "oops"\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	load[SecretMapConfig](files: [path]) or {
		msg := err.msg()
		assert !msg.contains('FAKE_FILE_MAP_KEY'), 'secret map input must not leak: ${msg}'
		assert msg.contains('`credentials`'), 'setting must still be named: ${msg}'
		assert msg.contains('redacted'), 'secret errors must note redaction: ${msg}'
		return
	}
	assert false, 'bad secret map value must error'
}

struct PlainMapConfig {
	limits map[string]int
}

fn test_config_plain_map_error_names_key() {
	load[PlainMapConfig](
		cli:        ['--limits=MYKEY=bad']
		env_prefix: 'VLIB_TEST_PMAP_UNSET_'
	) or {
		assert err.msg().contains('limits.MYKEY'), 'non-secret map errors name the key: ${err.msg()}'
		return
	}
	assert false, 'bad map value must error'
}

struct DatabaseConfig {
	host string = 'localhost'
	port int    = 5432
}

struct RequiredDbConfig {
	db DatabaseConfig @[required]
}

struct OptionalDbConfig {
	db DatabaseConfig
}

fn test_config_required_nested_struct_wrong_shape_errors() {
	path := os.join_path(os.temp_dir(), 'v_config_db_shape.toml')
	os.write_file(path, 'db = []\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	load[RequiredDbConfig](files: [path]) or {
		assert err.msg().contains('`db`'), err.msg()
		return
	}
	assert false, 'array for a required nested struct must error'
}

fn test_config_optional_nested_struct_wrong_shape_errors() {
	path := os.join_path(os.temp_dir(), 'v_config_db_shape_opt.toml')
	os.write_file(path, 'db = []\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	load[OptionalDbConfig](files: [path]) or {
		assert err.msg().contains('`db`'), err.msg()
		return
	}
	assert false, 'array for an optional nested struct must error'
}

fn test_config_nested_struct_shape_controls() {
	valid := os.join_path(os.temp_dir(), 'v_config_db_valid.toml')
	os.write_file(valid, '[db]\nhost = "real"\nport = 1234\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(valid) or {}
	}
	cfg := load[RequiredDbConfig](files: [valid])!
	assert cfg.db.host == 'real'
	assert cfg.db.port == 1234
	empty := os.join_path(os.temp_dir(), 'v_config_db_empty.toml')
	os.write_file(empty, '# empty\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(empty) or {}
	}
	load[RequiredDbConfig](files: [empty]) or {
		assert err.msg().contains('[missing]'), err.msg()
		return
	}
	assert false, 'missing required struct must error'
	opt := load[OptionalDbConfig](files: [empty])!
	assert opt.db.host == 'localhost', 'native defaults apply when optional struct is absent'
	assert opt.db.port == 5432
}

struct UnderscoreHttpConfig {
	read_timeout int = 30
}

struct UnderscoreServerConfig {
	http_server UnderscoreHttpConfig
	token       string @[default: 't']
}

fn test_config_nested_dotted_cli_with_underscores() {
	dotted := load[UnderscoreServerConfig](
		cli:        ['--http_server.read_timeout=90']
		env_prefix: 'VLIB_TEST_USCORE_UNSET_'
	)!
	assert dotted.http_server.read_timeout == 90, 'dotted underscore CLI must apply'
	dashed := load[UnderscoreServerConfig](
		cli:        ['--http-server.read-timeout=91']
		env_prefix: 'VLIB_TEST_USCORE_UNSET_'
	)!
	assert dashed.http_server.read_timeout == 91, 'dotted dashed CLI must apply'
	flat_dashed := load[UnderscoreServerConfig](
		cli:        ['--http-server-read-timeout=92']
		env_prefix: 'VLIB_TEST_USCORE_UNSET_'
	)!
	assert flat_dashed.http_server.read_timeout == 92, 'flat dashed CLI must apply'
	flat_score := load[UnderscoreServerConfig](
		cli:        ['--http_server_read_timeout=93']
		env_prefix: 'VLIB_TEST_USCORE_UNSET_'
	)!
	assert flat_score.http_server.read_timeout == 93, 'flat underscore CLI must apply'
}

struct RequiredUnderscoreConfig {
	http_server UnderscoreHttpConfig @[required]
}

fn test_config_required_nested_struct_dotted_cli_counts() {
	cfg := load[RequiredUnderscoreConfig](
		cli:        ['--http_server.read_timeout=90']
		env_prefix: 'VLIB_TEST_USCORE_REQ_'
	)!
	assert cfg.http_server.read_timeout == 90, 'dotted CLI must satisfy required nested struct'
}

fn test_config_nested_struct_higher_file_wins_over_malformed_lower() {
	high := os.join_path(os.temp_dir(), 'v_config_db_high.toml')
	os.write_file(high, '[db]\nhost = "high.db"\nport = 6000\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(high) or {}
	}
	low := os.join_path(os.temp_dir(), 'v_config_db_low.toml')
	os.write_file(low, 'db = []\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(low) or {}
	}
	cfg := load[RequiredDbConfig](files: [high, low])!
	assert cfg.db.host == 'high.db', 'first file must win over malformed lower file'
	assert cfg.db.port == 6000
	load[RequiredDbConfig](files: [low, high]) or {
		assert err.msg().contains('`db`'), err.msg()
		return
	}
	assert false, 'malformed first file must error even with valid fallback'
}

fn test_config_nested_struct_cli_beats_malformed_file() {
	low := os.join_path(os.temp_dir(), 'v_config_db_cli.toml')
	os.write_file(low, 'db = []\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(low) or {}
	}
	cfg := load[RequiredDbConfig](
		cli:   ['--db-host=cli.db', '--db-port=7000']
		files: [low]
	)!
	assert cfg.db.host == 'cli.db', 'complete cli must beat malformed file'
	assert cfg.db.port == 7000
	load[RequiredDbConfig](cli: ['--db-host=cli.db'], files: [low]) or {
		assert err.msg().contains('`db`'), 'partial cli must not silence malformed file: ${err.msg()}'
		return
	}
	assert false, 'partial cli override must still report malformed parent'
}

fn test_config_nested_struct_env_beats_malformed_file() {
	low := os.join_path(os.temp_dir(), 'v_config_db_env.toml')
	os.write_file(low, 'db = []\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(low) or {}
	}
	os.setenv('VLIB_TEST_PREC2_DB_HOST', 'env.db', true)
	os.setenv('VLIB_TEST_PREC2_DB_PORT', '8000', true)
	defer {
		os.unsetenv('VLIB_TEST_PREC2_DB_HOST')
		os.unsetenv('VLIB_TEST_PREC2_DB_PORT')
	}
	cfg := load[RequiredDbConfig](env_prefix: 'VLIB_TEST_PREC2_', files: [low])!
	assert cfg.db.host == 'env.db', 'complete env must beat malformed file'
	assert cfg.db.port == 8000
}

fn test_config_scalar_cli_beats_malformed_file() {
	path := os.join_path(os.temp_dir(), 'v_config_scalar_shape.toml')
	os.write_file(path, 'port = []\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(path) or {}
	}
	cfg := load[PrecConfig](
		cli:        ['--port=7000']
		env_prefix: 'VLIB_TEST_PREC_UNSET_'
		files:      [path]
	)!
	assert cfg.port == 7000, 'cli must beat malformed scalar file value'
}

struct InnerNested {
	host string = 'localhost'
	port int    = 5432
}

struct MidNested {
	db InnerNested
}

struct OuterNested {
	mid MidNested
}

fn test_config_two_level_nested_higher_wins_over_malformed_lower() {
	high := os.join_path(os.temp_dir(), 'v_config_two_high.toml')
	os.write_file(high, '[mid.db]\nhost = "high.db"\nport = 6000\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(high) or {}
	}
	low := os.join_path(os.temp_dir(), 'v_config_two_low.toml')
	os.write_file(low, 'mid = []\n') or {
		panic('write: ${err}')
	}
	defer {
		os.rm(low) or {}
	}
	cfg := load[OuterNested](files: [high, low])!
	assert cfg.mid.db.host == 'high.db', 'first file must win for two-level nesting'
	assert cfg.mid.db.port == 6000
	load[OuterNested](files: [low]) or {
		assert err.msg().contains('`mid`'), err.msg()
		return
	}
	assert false, 'malformed two-level parent must error when it would replace defaults'
}
