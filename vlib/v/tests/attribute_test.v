@[testing]
struct StructAttrTest {
	foo string
	bar int
}

@[testing]
pub struct PubStructAttrTest {
	foo string
	bar int
}

struct StructFieldAttrTest {
	foo string @[attr: bar; attr0; attr1: 'foo']
	bar int    @[attr0: 123; attr1: true; attr2: false]
	baz bool = true   @[prefix.attr0]
}

@[testing]
enum EnumAttrTest {
	one
	two
}

@[testing]
pub enum PubEnumAttrTest {
	one
	two
}

@[attrone; attrtwo]
pub enum EnumMultiAttrTest {
	one
	two
}

@[testing]
fn test_fn_attribute() {
	assert true
}

@[testing]
pub fn test_pub_fn_attribute() {
	assert true
}

@[attrone; attrtwo]
fn test_fn_multi_attribute() {
	assert true
}

@[attrone; attrtwo]
fn test_fn_multi_attribute_single() {
	assert true
}
