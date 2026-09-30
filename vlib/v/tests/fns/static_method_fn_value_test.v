// Static methods used as function values (`Type.method` without a call) must be
// emitted as the function itself in C, in every context: voidptr casts and
// comparisons, embedded struct fields, and generic bodies, where the cloned
// selector has no checker resolution.
struct Picker {}

fn Picker.on_remove(x int) int {
	return x + 1
}

fn Picker.on_add(x int) int {
	return x + 2
}

type PointFn = fn (int, int) int

struct Transform {
mut:
	on_point PointFn               = unsafe { nil }
	on_raw   fn (voidptr, int) int = unsafe { nil }
}

struct ClipTransform {
	Transform
}

fn ClipTransform.handle_point(x int, y int) int {
	return x * y
}

fn ClipTransform.handle_raw(_ voidptr, x int) int {
	return x * 3
}

fn is_remove(f fn (int) int) bool {
	return voidptr(f) == Picker.on_remove
}

fn test_static_method_value_as_voidptr() {
	assert is_remove(Picker.on_remove)
	assert !is_remove(Picker.on_add)
	p := voidptr(Picker.on_add)
	assert p != unsafe { nil }
	ptrs := [voidptr(Picker.on_remove), voidptr(Picker.on_add)]
	assert ptrs[0] != ptrs[1]
	assert voidptr(Picker.on_remove) == ptrs[0]
}

fn test_static_method_value_in_embedded_field() {
	mut t := &ClipTransform{}
	t.Transform.on_point = ClipTransform.handle_point
	assert t.on_point(3, 4) == 12
	t.on_point = PointFn(ClipTransform.handle_point)
	assert t.on_point(2, 5) == 10
}

fn setup_transform[T](mut t ClipTransform) {
	t.on_point = ClipTransform.handle_point
	t.Transform.on_raw = ClipTransform.handle_raw
}

fn pick_value[T](x T) int {
	f := Picker.on_remove
	fns := [Picker.on_add]
	return f(x) + fns[0](x)
}

fn remove_ptr[T]() voidptr {
	return voidptr(Picker.on_remove)
}

struct Registry[T] {
mut:
	cb fn (int) int = unsafe { nil }
}

fn (mut r Registry[T]) init() {
	r.cb = Picker.on_add
}

fn test_static_method_value_in_generic_bodies() {
	mut t := &ClipTransform{}
	setup_transform[int](mut t)
	assert t.on_point(3, 4) == 12
	assert t.on_raw(unsafe { nil }, 4) == 12
	assert pick_value[int](1) == 5
	assert remove_ptr[int]() == voidptr(Picker.on_remove)
	mut r := Registry[string]{}
	r.init()
	assert r.cb(1) == 3
}
