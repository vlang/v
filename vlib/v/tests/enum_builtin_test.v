import os

const (
	vdir = @VEXEROOT
	builtin_path = '$vdir/vlib/builtin/tmp_my_enum.v'
	builtin_content = 'module builtin
pub enum BuiltinTestEnum{
    v0 = 0
    v_t
}
pub fn (x BuiltinTestEnum) some_method() string {
    match x {
        .v0 { return "v0" }
        else { return "v_t" }
    }
}'
	genc_path = '$vdir/vlib/v/gen/c/tmp_builtin_enum_usage.v'
	genc_content = 'module c
pub fn builtin_enum_usage() {
    x := BuiltinTestEnum.v0
    println(x.some_method())
}'
)

fn testsuite_end() {
	os.rm(builtin_path) or {}
	os.rm(genc_path) or {}
}

fn test_enum_method() {
	os.write_file(builtin_path, builtin_content) or {panic(err)}
	os.write_file(genc_path, genc_content) or {panic(err)}
	res := os.execute('v self')
	assert res.exit_code == 0
}
