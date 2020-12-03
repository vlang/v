module builtin

pub struct JS.Number {}
pub struct JS.String {
	length JS.Number
}
pub struct JS.Boolean {}
pub struct JS.Array {}
pub struct JS.Map {}

fn (v JS.String) toString() JS.String
fn (v JS.Number) toString() JS.String
fn (v JS.Boolean) toString() JS.String
fn (v JS.Array) toString() JS.String
fn (v JS.Map) toString() JS.String

// V_ is a workaround as structs cant override basic types such as `i8` and `string`
// to add methods to these structs, use their normal name (aka without `V_`)
// this gets handled here:
//   gen/js/builtin_types.v
//   gen/js/js.v gen_struct_decl()

pub struct V_i8 {
	val JS.Number
}
fn (v V_i8) valueOf() JS.Number { return v.val }
fn (v V_i8) toString() JS.String { return v.val.toString() }

pub struct V_i16 {
	val JS.Number
}
fn (v V_i16) valueOf() JS.Number { return v.val }
fn (v V_i16) toString() JS.String { return v.val.toString() }

pub struct V_int {
	val JS.Number
}
fn (v V_int) valueOf() JS.Number { return v.val }
fn (v V_int) toString() JS.String { return v.val.toString() }

pub struct V_i64 {
	val JS.Number
}
fn (v V_i64) valueOf() JS.Number { return v.val }
fn (v V_i64) toString() JS.String { return v.val.toString() }

pub struct V_byte {
	val JS.Number
}
fn (v V_byte) valueOf() JS.Number { return v.val }
fn (v V_byte) toString() JS.String { return v.val.toString() }

pub struct V_u16 {
	val JS.Number
}
fn (v V_u16) valueOf() JS.Number { return v.val }
fn (v V_u16) toString() JS.String { return v.val.toString() }

pub struct V_u32 {
	val JS.Number
}
fn (v V_u32) valueOf() JS.Number { return v.val }
fn (v V_u32) toString() JS.String { return v.val.toString() }

pub struct V_u64 {
	val JS.Number
}
fn (v V_u64) valueOf() JS.Number { return v.val }
fn (v V_u64) toString() JS.String { return v.val.toString() }

pub struct V_f32 {
	val JS.Number
}
fn (v V_f32) valueOf() JS.Number { return v.val }
fn (v V_f32) toString() JS.String { return v.val.toString() }

pub struct V_f64 {
	val JS.Number
}
fn (v V_f64) valueOf() JS.Number { return v.val }
fn (v V_f64) toString() JS.String { return v.val.toString() }

pub struct V_any_int {
	val JS.Number
}
fn (v V_any_int) valueOf() JS.Number { return v.val }
fn (v V_any_int) toString() JS.String { return v.val.toString() }

pub struct V_any_float {
	val JS.Number
}
fn (v V_any_float) valueOf() JS.Number { return v.val }
fn (v V_any_float) toString() JS.String { return v.val.toString() }

pub struct V_size_t {
	val JS.Number
}
fn (v V_size_t) valueOf() JS.Number { return v.val }
fn (v V_size_t) toString() JS.String { return v.val.toString() }

pub struct V_bool {
	val JS.Boolean
}
fn (v V_bool) valueOf() JS.Boolean { return v.val }
fn (v V_bool) toString() JS.String { return v.val.toString() }

pub struct V_string {
	str JS.String
}
fn (v V_string) valueOf() JS.String { return v.str }
fn (v V_string) toString() JS.String { return v.str }

pub struct V_map {
	val JS.Map
}
fn (v V_map) valueOf() JS.Map { return v.val }
fn (v V_map) toString() JS.String { return v.val.toString() }

pub struct V_array {
	val JS.Array
}
fn (v V_array) valueOf() JS.Array { return v.val }
fn (v V_array) toString() JS.String { return v.val.toString() }