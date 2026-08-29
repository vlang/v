import os

const json_pointer_value_vexe = @VEXE
const json_pointer_value_tests_dir = os.dir(@FILE)
const json_pointer_value_v3_dir = os.dir(json_pointer_value_tests_dir)
const json_pointer_value_vlib_dir = os.dir(json_pointer_value_v3_dir)
const json_pointer_value_v3_src = os.join_path(json_pointer_value_v3_dir, 'v3.v')

fn test_json_voidptr_autoref_keeps_value_equality_semantics() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_json_pointer_value_${os.getpid()}')
	src := os.join_path(os.temp_dir(), 'v3_json_pointer_value_${os.getpid()}.v')
	bin := os.join_path(os.temp_dir(), 'v3_json_pointer_value_program_${os.getpid()}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(bin) or {}
	}
	build :=
		os.execute('"${json_pointer_value_vexe}" -old-compiler -gc none -prealloc -path "${json_pointer_value_vlib_dir}|@vlib|@vmodules" -o "${v3_bin}" "${json_pointer_value_v3_src}"')
	assert build.exit_code == 0, build.output

	os.write_file(src, 'import json

@[json_as_number]
enum JsonExactSigned as i64 {
	exact = 9_007_199_254_740_993
}

@[json_as_number]
enum JsonExactUnsigned as u64 {
	exact = 0xffff_ffff_ffff_ffff
}

struct JsonExactEnums {
	signed   JsonExactSigned
	unsigned JsonExactUnsigned
}

struct JsonPointerValueRegression {
	value i32
}

struct JsonPointerValueEnvelope[T] {
	value T
}

type JsonPointerValueAlias = JsonPointerValueRegression

fn decode_json_pointer_value[T](source string) !T {
	decoded := json.decode(JsonPointerValueEnvelope[T], source)!
	return decoded.value
}

fn decode_json_result_direct[T](source string) !T {
	return json.decode(T, source)
}

fn decode_json_result_local[T](source string) !T {
	decoded := json.decode(T, source)!
	return decoded
}

fn ordinary_alias_result() !JsonPointerValueAlias {
	return JsonPointerValueAlias(JsonPointerValueRegression{
		value: 9
	})
}

fn main() {
	exact_enums := json.decode(JsonExactEnums,
		\'{"signed":9007199254740993,"unsigned":18446744073709551615}\')!
	assert i64(exact_enums.signed) == i64(9_007_199_254_740_993)
	assert u64(exact_enums.unsigned) == u64(0xffff_ffff_ffff_ffff)
	default_enums := json.decode(JsonExactEnums, \'{}\')!
	assert default_enums.signed == .exact
	assert default_enums.unsigned == .exact
	mut decoded := json.decode(JsonPointerValueRegression, \'{"value":42}\')!
	assert decoded == JsonPointerValueRegression{
		value: 42
	}
	decoded = json.decode(JsonPointerValueRegression, \'{"value":43}\')!
	assert decoded.value == 43
	assert json.encode(decoded) == \'{"value":43}\'
	assert json.decode(JsonPointerValueRegression, \'{"value":44}\')!.str() == \'JsonPointerValueRegression{\n    value: 44\n}\'
	assert decode_json_pointer_value[int](\'{"value":7}\')! == 7
	direct_result := decode_json_result_direct[JsonPointerValueEnvelope[int]](\'{"value":10}\')!
	assert direct_result.value == 10
	local_result := decode_json_result_local[JsonPointerValueRegression](\'{"value":11}\')!
	assert local_result.value == 11
	decoded_alias := json.decode(JsonPointerValueAlias, \'{"value":8}\')!
	assert typeof(decoded_alias).name == \'JsonPointerValueAlias\'
	assert decoded_alias.str().starts_with(\'JsonPointerValueAlias(\')
	ordinary_alias := ordinary_alias_result()!
	assert typeof(ordinary_alias).name == \'JsonPointerValueAlias\'
	assert ordinary_alias.str().starts_with(\'JsonPointerValueAlias(\')
	anon := json.decode(struct {
		label string
	}, \'{"label":"ok"}\')!
	assert anon.label == \'ok\'
	println(\'ok\')
}
') or {
		panic(err)
	}
	compile := os.execute('"${v3_bin}" -silent -gc none -nocache -no-parallel "${src}" -o "${bin}"')
	assert compile.exit_code == 0, compile.output
	run := os.execute('"${bin}"')
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
