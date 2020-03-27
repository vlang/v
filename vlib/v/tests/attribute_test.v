[testing]
struct StructAttrTest {
	foo string
	bar int
}

[testing]
pub struct PubStructAttrTest {
	foo string
	bar int
}

[testing]
enum EnumAttrTest {
	one
	two
}

[testing]
pub enum PubEnumAttrTest {
	one
	two
}

[testing]
fn test_fn_attribute() {
	assert true
}

[testing]
pub fn test_pub_fn_attribute() {
	assert true
}
