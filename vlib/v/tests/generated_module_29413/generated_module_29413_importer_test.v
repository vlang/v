import os
import zbrgen

// An ordinary file keeps V's naming rules when it imports a `@[generated]` module,
// and `mod.value.member` stays a value access in it.
fn test_ordinary_file_importing_a_generated_module() {
	assert os.args.len > 0
	assert zbrgen.zbrNames.len == 2
	assert zbrgen._zbr_c_Origin.xPos == 2
	assert zbrgen._zbr_c_Origin.scaledSum() == 50
	assert zbrgen._zbr_fn_origin().xPos == 0
	mode := zbrgen._zbr_ty_Mode.fastMode
	assert mode == .fastMode
	assert zbrgen._zbr_fn_describe(zbrgen._zbr_ty_Value(mode)) == 'mode fastMode'
}
