module builtin

pub struct JS.Number {}
pub struct JS.String {
	length JS.Number
}
pub struct JS.Boolean {}
pub struct JS.Array {}
pub struct JS.Map {}

// V_ is a workaround as structs cant override basic types such as `i8` and `string`
// to add methods to these structs, use their normal name (aka without `V_`)
// this gets handled here:
//   gen/js/builtin_types.v
//   gen/js/js.v gen_struct_decl()
// 
pub struct V_i8 {
	val JS.Number
}
pub struct V_i16 {
	val JS.Number
}
pub struct V_int {
	val JS.Number
}
pub struct V_i64 {
	val JS.Number
}
pub struct V_byte {
	val JS.Number
}
pub struct V_u16 {
	val JS.Number
}
pub struct V_u32 {
	val JS.Number
}
pub struct V_u64 {
	val JS.Number
}
pub struct V_f32 {
	val JS.Number
}
pub struct V_f64 {
	val JS.Number
}
pub struct V_any_int {
	val JS.Number
}
pub struct V_any_float {
	val JS.Number
}
pub struct V_size_t {
	val JS.Number
}
pub struct V_bool {
	val JS.Boolean
}
pub struct V_string {
	val JS.String
}
pub struct V_map {
	val JS.Map
}
pub struct V_array {
	val JS.Array
}