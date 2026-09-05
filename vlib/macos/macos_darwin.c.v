module macos

#flag darwin -framework Foundation

#flag darwin -lobjc

#insert "@VEXEROOT/vlib/macos/objc_bridge.h"

pub type Id = voidptr

pub type Sel = voidptr

pub type Class = voidptr

pub type Protocol = voidptr

// Point is a Cocoa-compatible point or size containing two f64 values.
pub type Point = C.macos_point

// Range is a Cocoa-compatible range containing a location and length.
pub type Range = C.macos_range

pub type Rect = C.macos_rect

pub struct C.macos_point {
pub mut:
	x f64
	y f64
}

pub struct C.macos_range {
pub mut:
	location u64
	length   u64
}

pub struct C.macos_rect {
pub mut:
	x      f64
	y      f64
	width  f64
	height f64
}

pub const assoc_assign = usize(0)
pub const assoc_retain_nonatomic = usize(1)
pub const assoc_copy_nonatomic = usize(3)
pub const assoc_retain = usize(0o1401)
pub const assoc_copy = usize(0o1403)
pub const run_loop_default_mode = 'kCFRunLoopDefaultMode'

fn C.macos_objc_msg_id0(obj Id, selector Sel) Id

fn C.macos_objc_msg_id1(obj Id, selector Sel, a0 voidptr) Id

fn C.macos_objc_msg_id2(obj Id, selector Sel, a0 voidptr, a1 voidptr) Id

fn C.macos_objc_msg_id3(obj Id, selector Sel, a0 voidptr, a1 voidptr, a2 voidptr) Id

fn C.macos_objc_msg_id4(obj Id, selector Sel, a0 voidptr, a1 voidptr, a2 voidptr, a3 voidptr) Id

fn C.macos_objc_msg_id_rect(obj Id, selector Sel, rect Rect) Id

fn C.macos_objc_msg_id_rect_bool(obj Id, selector Sel, rect Rect, a1 bool) Id

fn C.macos_objc_msg_id_rect_obj(obj Id, selector Sel, rect Rect, a1 voidptr) Id

fn C.macos_objc_msg_id_rect_u64_u64_bool(obj Id, selector Sel, rect Rect, a1 u64, a2 u64, a3 bool) Id

fn C.macos_objc_msg_id_obj_u64_bool(obj Id, selector Sel, a0 voidptr, a1 u64, a2 bool) Id

fn C.macos_objc_msg_id_obj_sel_obj(obj Id, selector Sel, a0 voidptr, a1 voidptr, a2 voidptr) Id

fn C.macos_objc_msg_id_f64(obj Id, selector Sel, a0 f64) Id

fn C.macos_objc_msg_id_u64(obj Id, selector Sel, a0 u64) Id

fn C.macos_objc_msg_id_four_f64(obj Id, selector Sel, a0 f64, a1 f64, a2 f64, a3 f64) Id

fn C.macos_objc_msg_id_id_f64(obj Id, selector Sel, a0 Id, a1 f64) Id

fn C.macos_objc_msg_id_id_u64(obj Id, selector Sel, a0 Id, a1 u64) Id

fn C.macos_objc_msg_id_id_u64_i64_f64(obj Id, selector Sel, a0 Id, a1 u64, a2 i64, a3 f64) Id

fn C.macos_objc_msg_id_u64_id(obj Id, selector Sel, a0 u64, a1 Id) Id

fn C.macos_objc_msg_id_u64_range_ptr(obj Id, selector Sel, a0 u64, range &Range) Id

fn C.macos_objc_msg_id_id_u64_range_ptr(obj Id, selector Sel, a0 Id, a1 u64, range &Range) Id

fn C.macos_objc_msg_id_range(obj Id, selector Sel, range Range) Id

fn C.macos_objc_msg_u64_id(obj Id, selector Sel, a0 Id) u64

fn C.macos_objc_msg_bool_id_bool(obj Id, selector Sel, a0 Id, a1 bool) bool

fn C.macos_objc_msg_bool_sel_id_id(obj Id, selector Sel, a0 Sel, a1 Id, a2 Id) bool

fn C.macos_objc_msg_range(obj Id, selector Sel) Range

fn C.macos_objc_msg_point(obj Id, selector Sel) Point

fn C.macos_objc_msg_point_point_id(obj Id, selector Sel, point Point, a1 Id) Point

fn C.macos_objc_msg_void0(obj Id, selector Sel)

fn C.macos_objc_msg_void1(obj Id, selector Sel, a0 voidptr)

fn C.macos_objc_msg_void2(obj Id, selector Sel, a0 voidptr, a1 voidptr)

fn C.macos_objc_msg_void3(obj Id, selector Sel, a0 voidptr, a1 voidptr, a2 voidptr)

fn C.macos_objc_msg_void_bool(obj Id, selector Sel, a0 bool)

fn C.macos_objc_msg_void_i64(obj Id, selector Sel, a0 i64)

fn C.macos_objc_msg_void_u64(obj Id, selector Sel, a0 u64)

fn C.macos_objc_msg_void_f64(obj Id, selector Sel, a0 f64)

fn C.macos_objc_msg_void_rect(obj Id, selector Sel, rect Rect)

fn C.macos_objc_msg_void_rect_bool_bool(obj Id, selector Sel, rect Rect, a1 bool, a2 bool)

fn C.macos_objc_msg_void_id_i64_id(obj Id, selector Sel, a0 Id, a1 i64, a2 Id)

fn C.macos_objc_msg_void_id_range(obj Id, selector Sel, a0 Id, range Range)

fn C.macos_objc_msg_void_id_id_range(obj Id, selector Sel, a0 Id, a1 Id, range Range)

fn C.macos_objc_msg_void_range(obj Id, selector Sel, range Range)

fn C.macos_objc_msg_void_rect_id(obj Id, selector Sel, rect Rect, a1 Id)

fn C.macos_objc_msg_void_point(obj Id, selector Sel, point Point)

fn C.macos_objc_msg_void_id_sel_id_id(obj Id, selector Sel, a0 Id, a1 Sel, a2 Id, a3 Id)

fn C.macos_objc_msg_void_sel_id_bool(obj Id, selector Sel, a0 Sel, a1 Id, a2 bool)

fn C.macos_objc_msg_bool0(obj Id, selector Sel) bool

fn C.macos_objc_msg_bool1(obj Id, selector Sel, a0 voidptr) bool

fn C.macos_objc_msg_i64(obj Id, selector Sel) i64

fn C.macos_objc_msg_u64(obj Id, selector Sel) u64

fn C.macos_objc_msg_f64(obj Id, selector Sel) f64

fn C.macos_objc_msg_rect(obj Id, selector Sel) Rect

fn C.macos_objc_get_class(const_name &char) Class

fn C.macos_sel_register_name(const_name &char) Sel

fn C.macos_objc_allocate_class_pair(superclass Class, const_name &char, extra_bytes usize) Class

fn C.macos_objc_register_class_pair(cls Class)

fn C.macos_class_add_method(cls Class, name Sel, imp voidptr, const_types &char) bool

fn C.macos_class_add_ivar(cls Class, const_name &char, size usize, alignment u8, const_types &char) bool

fn C.macos_objc_get_protocol(const_name &char) Protocol

fn C.macos_class_add_protocol(cls Class, protocol Protocol) bool

fn C.macos_objc_set_ptr_ivar(obj Id, const_name &char, value voidptr)

fn C.macos_objc_get_ptr_ivar(obj Id, const_name &char) voidptr

fn C.macos_set_associated_object(obj Id, const_key voidptr, value Id, policy usize)

fn C.macos_get_associated_object(obj Id, const_key voidptr) Id

@[inline]
pub fn get_class(name string) Id {
	return Id(C.macos_objc_get_class(&char(name.str)))
}

@[inline]
pub fn get_protocol(name string) Protocol {
	return C.macos_objc_get_protocol(&char(name.str))
}

@[inline]
pub fn sel(name string) Sel {
	return C.macos_sel_register_name(&char(name.str))
}

@[inline]
pub fn rect(x f64, y f64, width f64, height f64) Rect {
	return C.macos_rect{
		x: x
		y: y
		width: width
		height: height
	}
}

// point returns a Cocoa-compatible point with the supplied coordinates.
@[inline]
pub fn point(x f64, y f64) Point {
	return C.macos_point{
		x: x
		y: y
	}
}

// range returns a Cocoa-compatible range with the supplied location and length.
@[inline]
pub fn range(location u64, length u64) Range {
	return C.macos_range{
		location: location
		length: length
	}
}

@[inline]
pub fn msg_id(obj Id, selector string) Id {
	return C.macos_objc_msg_id0(obj, sel(selector))
}

@[inline]
pub fn msg_void(obj Id, selector string) {
	C.macos_objc_msg_void0(obj, sel(selector))
}

@[inline]
pub fn msg_bool(obj Id, selector string) bool {
	return C.macos_objc_msg_bool0(obj, sel(selector))
}

@[inline]
pub fn msg_i64(obj Id, selector string) i64 {
	return C.macos_objc_msg_i64(obj, sel(selector))
}

@[inline]
pub fn msg_u64(obj Id, selector string) u64 {
	return C.macos_objc_msg_u64(obj, sel(selector))
}

@[inline]
pub fn msg_f64(obj Id, selector string) f64 {
	return C.macos_objc_msg_f64(obj, sel(selector))
}

@[inline]
pub fn msg_rect(obj Id, selector string) Rect {
	return C.macos_objc_msg_rect(obj, sel(selector))
}

// ── Multi-argument message sending ─────────────────────────────────

@[inline]
pub fn msg_id1(obj Id, selector string, a0 Id) Id {
	return C.macos_objc_msg_id1(obj, sel(selector), a0)
}

@[inline]
pub fn msg_id2(obj Id, selector string, a0 Id, a1 Id) Id {
	return C.macos_objc_msg_id2(obj, sel(selector), a0, a1)
}

@[inline]
pub fn msg_id3(obj Id, selector string, a0 Id, a1 Id, a2 Id) Id {
	return C.macos_objc_msg_id3(obj, sel(selector), a0, a1, a2)
}

@[inline]
pub fn msg_id4(obj Id, selector string, a0 Id, a1 Id, a2 Id, a3 Id) Id {
	return C.macos_objc_msg_id4(obj, sel(selector), a0, a1, a2, a3)
}

@[inline]
pub fn msg_id_rect(obj Id, selector string, r Rect) Id {
	return C.macos_objc_msg_id_rect(obj, sel(selector), r)
}

@[inline]
pub fn msg_id_f64(obj Id, selector string, a0 f64) Id {
	return C.macos_objc_msg_id_f64(obj, sel(selector), a0)
}

@[inline]
pub fn msg_id_u64(obj Id, selector string, a0 u64) Id {
	return C.macos_objc_msg_id_u64(obj, sel(selector), a0)
}

// msg_id_rect_u64_u64_bool sends a message that returns an object and accepts a rectangle,
// two unsigned integers, and a boolean.
@[inline]
pub fn msg_id_rect_u64_u64_bool(obj Id, selector string, rect Rect, a1 u64, a2 u64, a3 bool) Id {
	return C.macos_objc_msg_id_rect_u64_u64_bool(obj, sel(selector), rect, a1, a2, a3)
}

// msg_id_four_f64 sends a message that returns an object and accepts four f64 values.
@[inline]
pub fn msg_id_four_f64(obj Id, selector string, a0 f64, a1 f64, a2 f64, a3 f64) Id {
	return C.macos_objc_msg_id_four_f64(obj, sel(selector), a0, a1, a2, a3)
}

// msg_id_id_f64 sends a message that returns an object and accepts an object and an f64.
@[inline]
pub fn msg_id_id_f64(obj Id, selector string, a0 Id, a1 f64) Id {
	return C.macos_objc_msg_id_id_f64(obj, sel(selector), a0, a1)
}

// msg_id_id_u64 sends a message that returns an object and accepts an object and a u64.
@[inline]
pub fn msg_id_id_u64(obj Id, selector string, a0 Id, a1 u64) Id {
	return C.macos_objc_msg_id_id_u64(obj, sel(selector), a0, a1)
}

// msg_id_id_u64_i64_f64 sends a message that returns an object and accepts mixed numeric arguments.
@[inline]
pub fn msg_id_id_u64_i64_f64(obj Id, selector string, a0 Id, a1 u64, a2 i64, a3 f64) Id {
	return C.macos_objc_msg_id_id_u64_i64_f64(obj, sel(selector), a0, a1, a2, a3)
}

// msg_id_u64_id sends a message that returns an object and accepts a u64 and an object.
@[inline]
pub fn msg_id_u64_id(obj Id, selector string, a0 u64, a1 Id) Id {
	return C.macos_objc_msg_id_u64_id(obj, sel(selector), a0, a1)
}

// msg_id_u64_range_ptr sends a message with a u64 and a writable range pointer.
@[inline]
pub fn msg_id_u64_range_ptr(obj Id, selector string, a0 u64, range_ptr &Range) Id {
	return C.macos_objc_msg_id_u64_range_ptr(obj, sel(selector), a0, range_ptr)
}

// msg_id_id_u64_range_ptr sends a message with an object, a u64, and a writable range pointer.
@[inline]
pub fn msg_id_id_u64_range_ptr(obj Id, selector string, a0 Id, a1 u64, range_ptr &Range) Id {
	return C.macos_objc_msg_id_id_u64_range_ptr(obj, sel(selector), a0, a1, range_ptr)
}

// msg_id_range sends a message that returns an object and accepts a range.
@[inline]
pub fn msg_id_range(obj Id, selector string, value Range) Id {
	return C.macos_objc_msg_id_range(obj, sel(selector), value)
}

// msg_u64_id sends a message that returns a u64 and accepts an object.
@[inline]
pub fn msg_u64_id(obj Id, selector string, a0 Id) u64 {
	return C.macos_objc_msg_u64_id(obj, sel(selector), a0)
}

// msg_bool_id sends a message that returns a boolean and accepts an object.
@[inline]
pub fn msg_bool_id(obj Id, selector string, a0 Id) bool {
	return C.macos_objc_msg_bool1(obj, sel(selector), a0)
}

// msg_bool_id_bool sends a message that returns a boolean and accepts an object and a boolean.
@[inline]
pub fn msg_bool_id_bool(obj Id, selector string, a0 Id, a1 bool) bool {
	return C.macos_objc_msg_bool_id_bool(obj, sel(selector), a0, a1)
}

// responds_to reports whether obj responds to selector.
@[inline]
pub fn responds_to(obj Id, selector string) bool {
	return C.macos_objc_msg_bool1(obj, sel('respondsToSelector:'), sel(selector))
}

// msg_bool_sel_id_id sends a message that returns a boolean and accepts a selector and two objects.
@[inline]
pub fn msg_bool_sel_id_id(obj Id, selector string, a0 Sel, a1 Id, a2 Id) bool {
	return C.macos_objc_msg_bool_sel_id_id(obj, sel(selector), a0, a1, a2)
}

// msg_range sends a message that returns a range.
@[inline]
pub fn msg_range(obj Id, selector string) Range {
	return C.macos_objc_msg_range(obj, sel(selector))
}

// msg_point sends a message that returns a point.
@[inline]
pub fn msg_point(obj Id, selector string) Point {
	return C.macos_objc_msg_point(obj, sel(selector))
}

// msg_point_point_id sends a message that returns a point and accepts a point and an object.
@[inline]
pub fn msg_point_point_id(obj Id, selector string, value Point, a1 Id) Point {
	return C.macos_objc_msg_point_point_id(obj, sel(selector), value, a1)
}

@[inline]
pub fn msg_void1(obj Id, selector string, a0 Id) {
	C.macos_objc_msg_void1(obj, sel(selector), a0)
}

@[inline]
pub fn msg_void2(obj Id, selector string, a0 Id, a1 Id) {
	C.macos_objc_msg_void2(obj, sel(selector), a0, a1)
}

@[inline]
pub fn msg_void3(obj Id, selector string, a0 Id, a1 Id, a2 Id) {
	C.macos_objc_msg_void3(obj, sel(selector), a0, a1, a2)
}

@[inline]
pub fn msg_void_bool(obj Id, selector string, a0 bool) {
	C.macos_objc_msg_void_bool(obj, sel(selector), a0)
}

@[inline]
pub fn msg_void_i64(obj Id, selector string, a0 i64) {
	C.macos_objc_msg_void_i64(obj, sel(selector), a0)
}

@[inline]
pub fn msg_void_u64(obj Id, selector string, a0 u64) {
	C.macos_objc_msg_void_u64(obj, sel(selector), a0)
}

@[inline]
pub fn msg_void_f64(obj Id, selector string, a0 f64) {
	C.macos_objc_msg_void_f64(obj, sel(selector), a0)
}

@[inline]
pub fn msg_void_rect(obj Id, selector string, r Rect) {
	C.macos_objc_msg_void_rect(obj, sel(selector), r)
}

// msg_void_id_i64_id sends a void message with an object, an i64, and an object.
@[inline]
pub fn msg_void_id_i64_id(obj Id, selector string, a0 Id, a1 i64, a2 Id) {
	C.macos_objc_msg_void_id_i64_id(obj, sel(selector), a0, a1, a2)
}

// msg_void_id_range sends a void message with an object and a range.
@[inline]
pub fn msg_void_id_range(obj Id, selector string, a0 Id, value Range) {
	C.macos_objc_msg_void_id_range(obj, sel(selector), a0, value)
}

// msg_void_id_id_range sends a void message with two objects and a range.
@[inline]
pub fn msg_void_id_id_range(obj Id, selector string, a0 Id, a1 Id, value Range) {
	C.macos_objc_msg_void_id_id_range(obj, sel(selector), a0, a1, value)
}

// msg_void_range sends a void message with a range.
@[inline]
pub fn msg_void_range(obj Id, selector string, value Range) {
	C.macos_objc_msg_void_range(obj, sel(selector), value)
}

// msg_void_rect_id sends a void message with a rectangle and an object.
@[inline]
pub fn msg_void_rect_id(obj Id, selector string, rect Rect, a1 Id) {
	C.macos_objc_msg_void_rect_id(obj, sel(selector), rect, a1)
}

// msg_void_point sends a void message with a point.
@[inline]
pub fn msg_void_point(obj Id, selector string, value Point) {
	C.macos_objc_msg_void_point(obj, sel(selector), value)
}

// msg_void_id_sel_id_id sends a void message with an object, a selector, and two objects.
@[inline]
pub fn msg_void_id_sel_id_id(obj Id, selector string, a0 Id, a1 Sel, a2 Id, a3 Id) {
	C.macos_objc_msg_void_id_sel_id_id(obj, sel(selector), a0, a1, a2, a3)
}

// msg_void_sel_id_bool sends a void message with a selector, an object, and a boolean.
@[inline]
pub fn msg_void_sel_id_bool(obj Id, selector string, a0 Sel, a1 Id, a2 bool) {
	C.macos_objc_msg_void_sel_id_bool(obj, sel(selector), a0, a1, a2)
}

// ── Runtime class helpers ──────────────────────────────────────────

@[inline]
pub fn allocate_class_pair(superclass Id, name string) Id {
	return C.macos_objc_allocate_class_pair(superclass, &char(name.str), 0)
}

@[inline]
pub fn register_class_pair(cls Id) {
	C.macos_objc_register_class_pair(cls)
}

@[inline]
pub fn add_method(cls Id, sel_name string, imp voidptr, types string) bool {
	return C.macos_class_add_method(cls, sel(sel_name), imp, &char(types.str))
}

@[inline]
pub fn add_protocol(cls Id, proto Protocol) bool {
	return C.macos_class_add_protocol(cls, proto)
}

// ── Allocation ─────────────────────────────────────────────────────

@[inline]
pub fn alloc(class_name string) Id {
	return msg_id(get_class(class_name), 'alloc')
}

@[inline]
pub fn new(class_name string) Id {
	return msg_id(get_class(class_name), 'new')
}

@[inline]
pub fn nsstring(s string) Id {
	return C.macos_objc_msg_id1(get_class('NSString'), sel('stringWithUTF8String:'), &char(s.str))
}

pub fn utf8_string(obj Id) string {
	if obj == unsafe { nil } {
		return ''
	}
	cstr := &char(C.macos_objc_msg_id0(obj, sel('UTF8String')))
	if cstr == unsafe { nil } {
		return ''
	}
	return unsafe { cstr.vstring().clone() }
}

pub fn description_string(obj Id) string {
	if obj == unsafe { nil } {
		return ''
	}
	return utf8_string(C.macos_objc_msg_id0(obj, sel('description')))
}

@[inline]
pub fn retain(obj Id) Id {
	if obj == unsafe { nil } {
		return unsafe { nil }
	}
	return C.macos_objc_msg_id0(obj, sel('retain'))
}

@[inline]
pub fn release(obj Id) {
	if obj == unsafe { nil } {
		return
	}
	C.macos_objc_msg_void0(obj, sel('release'))
}

@[inline]
pub fn autorelease_pool_new() Id {
	return new('NSAutoreleasePool')
}

@[inline]
pub fn set_associated_object(obj Id, key voidptr, value Id, policy usize) {
	C.macos_set_associated_object(obj, key, value, policy)
}

@[inline]
pub fn get_associated_object(obj Id, key voidptr) Id {
	return C.macos_get_associated_object(obj, key)
}
