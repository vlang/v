// Declare the custom metadata used by these syntax fixtures for strict builds.
@[attribute]
struct Testing {}

@[attribute]
struct Prefix {}

@[attribute]
struct Attr {
	value string
}

@[attribute]
struct Attr0 {
	value int
}

@[attribute]
struct Attr1 {
	value string
}

@[attribute]
struct AttrBool {
	value bool
}

@[attribute]
struct Attr2 {
	value bool
}

@[attribute]
struct AttrOne {}

@[attribute]
struct AttrTwo {}

@[Testing]
struct StructAttrTest {
	foo string
	bar int
}

@[Testing]
pub struct PubStructAttrTest {
	foo string
	bar int
}

struct StructFieldAttrTest {
	foo string      @[Attr: 'bar'; Testing; Attr1: 'foo']
	bar int         @[Attr0: 123; AttrBool: true; Attr2: false]
	baz bool = true @[Prefix; Testing]
}

@[Testing]
enum EnumAttrTest {
	one
	two
}

@[Testing]
pub enum PubEnumAttrTest {
	one
	two
}

@[AttrOne; AttrTwo]
pub enum EnumMultiAttrTest {
	one
	two
}

@[Testing]
fn test_fn_attribute() {
	assert true
}

@[Testing]
pub fn test_pub_fn_attribute() {
	assert true
}

@[AttrOne; AttrTwo]
fn test_fn_multi_attribute() {
	assert true
}

@[AttrOne; AttrTwo]
fn test_fn_multi_attribute_single() {
	assert true
}
