module macos

#flag darwin -framework Foundation
#flag darwin -lobjc
#insert "@VEXEROOT/vlib/macos/objc_bridge.h"

pub type Id = voidptr
pub type Sel = voidptr
pub type Class = voidptr
pub type Protocol = voidptr
pub type Rect = C.macos_rect

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
fn C.macos_objc_msg_bool0(obj Id, selector Sel) bool
fn C.macos_objc_msg_bool1(obj Id, selector Sel, a0 voidptr) bool
fn C.macos_objc_msg_i64(obj Id, selector Sel) i64
fn C.macos_objc_msg_u64(obj Id, selector Sel) u64
fn C.macos_objc_msg_f64(obj Id, selector Sel) f64
fn C.macos_objc_msg_rect(obj Id, selector Sel) Rect
fn C.macos_objc_get_class(name &char) Class
fn C.macos_sel_register_name(name &char) Sel
fn C.macos_objc_allocate_class_pair(superclass Class, name &char, extra_bytes usize) Class
fn C.macos_objc_register_class_pair(cls Class)
fn C.macos_class_add_method(cls Class, name Sel, imp voidptr, types &char) bool
fn C.macos_class_add_ivar(cls Class, name &char, size usize, alignment u8, types &char) bool
fn C.macos_objc_get_protocol(name &char) Protocol
fn C.macos_class_add_protocol(cls Class, protocol Protocol) bool
fn C.macos_objc_set_ptr_ivar(obj Id, name &char, value voidptr)
fn C.macos_objc_get_ptr_ivar(obj Id, name &char) voidptr
fn C.macos_set_associated_object(obj Id, key voidptr, value Id, policy usize)
fn C.macos_get_associated_object(obj Id, key voidptr) Id

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
		x:      x
		y:      y
		width:  width
		height: height
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
