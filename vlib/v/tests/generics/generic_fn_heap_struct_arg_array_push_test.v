// Regression test for https://github.com/vlang/v/issues/29948:
// a `@[heap]` struct that a generic function passed to a method was pushed
// into `[]HeapFrame` as a pointer.
// Every `add_frame` below has a single caller: the body of a method that a
// function without type parameters calls as well was lowered once, and that
// hid the bug.
@[heap]
struct HeapFrame {
mut:
	name string
}

struct PlainFrame {
mut:
	name string
}

struct RefArgTag {
mut:
	others []HeapFrame
}

fn (mut tag RefArgTag) add_frame(frame HeapFrame) {
	tag.others << frame
}

fn fill_with_ref[T](mut tag T) {
	mut cur := &HeapFrame{}
	cur.name = 'COMM'
	tag.add_frame(cur)
	cur.name = 'TIT2'
	tag.add_frame(cur)
	cur.name = 'changed after the push'
}

struct ValueArgTag {
mut:
	others []HeapFrame
}

fn (mut tag ValueArgTag) add_frame(frame HeapFrame) {
	tag.others << frame
}

fn fill_with_value[T](mut tag T) {
	mut cur := HeapFrame{}
	cur.name = 'COMM'
	tag.add_frame(cur)
	cur.name = 'TIT2'
	tag.add_frame(cur)
	cur.name = 'changed after the push'
}

struct PlainTag {
mut:
	others []PlainFrame
}

fn (mut tag PlainTag) add_frame(frame PlainFrame) {
	tag.others << frame
}

fn fill_plain[T](mut tag T) {
	mut cur := &PlainFrame{}
	cur.name = 'COMM'
	tag.add_frame(cur)
	cur.name = 'TIT2'
	tag.add_frame(cur)
	cur.name = 'changed after the push'
}

struct DirectTag {
mut:
	others []HeapFrame
}

fn (mut tag DirectTag) add_frame(frame HeapFrame) {
	tag.others << frame
}

fn fill_directly(mut tag DirectTag) {
	mut cur := &HeapFrame{}
	cur.name = 'COMM'
	tag.add_frame(cur)
	cur.name = 'TIT2'
	tag.add_frame(cur)
	cur.name = 'changed after the push'
}

fn test_heap_struct_ref_passed_by_value_from_generic_fn_is_pushed_as_value() {
	mut tag := RefArgTag{}
	fill_with_ref[RefArgTag](mut tag)
	assert tag.others.len == 2
	assert tag.others[0].name.len == 4
	assert tag.others[0].name == 'COMM'
	assert tag.others[1].name == 'TIT2'
}

fn test_heap_struct_value_passed_from_generic_fn_is_pushed_as_value() {
	mut tag := ValueArgTag{}
	fill_with_value[ValueArgTag](mut tag)
	assert tag.others.len == 2
	assert tag.others[0].name.len == 4
	assert tag.others[0].name == 'COMM'
	assert tag.others[1].name == 'TIT2'
}

fn test_plain_struct_ref_passed_by_value_from_generic_fn_is_pushed_as_value() {
	mut tag := PlainTag{}
	fill_plain[PlainTag](mut tag)
	assert tag.others.len == 2
	assert tag.others[0].name.len == 4
	assert tag.others[0].name == 'COMM'
	assert tag.others[1].name == 'TIT2'
}

fn test_heap_struct_ref_passed_by_value_from_plain_fn_is_pushed_as_value() {
	mut tag := DirectTag{}
	fill_directly(mut tag)
	assert tag.others.len == 2
	assert tag.others[0].name.len == 4
	assert tag.others[0].name == 'COMM'
	assert tag.others[1].name == 'TIT2'
}
