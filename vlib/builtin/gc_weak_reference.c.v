module builtin

// GCWeakReference is a copyable weak handle to a collectable Boehm object.
// Copies share a stable collector-owned cell. Keeping the handle alive keeps
// that cell alive, but does not keep its target alive. The zero handle is empty.
// There is no explicit free: the collector owns the registered cell's lifetime.
pub struct GCWeakReference {
	cell voidptr
}

struct GCWeakCell {
mut:
	target voidptr
}

@[c_extern]
fn C.GC_base(voidptr) voidptr

@[c_extern]
fn C.GC_get_find_leak() i32

@[c_extern]
fn C.GC_general_register_disappearing_link(link &voidptr, const_object voidptr) i32

@[c_extern]
fn C.GC_call_with_alloc_lock(callback fn (voidptr) voidptr, data voidptr) voidptr

// gc_weak_reference tries to create a weak handle to target. It returns none
// for nil, a non-GC pointer, an interior pointer, allocation/registration failure,
// non-Boehm collectors, or leak-finding mode. Ordinary and optimized Boehm
// modes are supported. Unsupported modes never substitute a strong reference.
//
// The caller must hold target strongly alive throughout this call. target must
// be the base of a live collectable GC allocation, not arena storage or an
// uncollectable allocation. GC_base cannot prove liveness or these allocation
// properties. Neither target nor the weak cell may be explicitly freed: Boehm
// does not guarantee clearing weak links when targets are manually freed.
@[unsafe]
pub fn gc_weak_reference(target voidptr) ?GCWeakReference {
	$if gcboehm ?&& !gcboehm_leak ? {
		if target == unsafe { nil } { return none }
		mut find_leak := false
		C.GC_call_with_alloc_lock(gc_weak_read_find_leak, &find_leak)
		if find_leak || C.GC_base(target) != target { return none }
		// vcalloc_noscan still scans under plain Boehm. This cell must always
		// be atomic and must bypass the current arena/preallocation scope.
		mut cell := unsafe { &GCWeakCell(C.GC_MALLOC_ATOMIC(uint(sizeof(GCWeakCell)))) }
		if cell == unsafe { nil } { return none }
		_ht_alloc(unsafe { &u8(cell) }, int(sizeof(GCWeakCell)))
		unsafe { *cell = GCWeakCell{ target: target } }
		if C.GC_general_register_disappearing_link(&cell.target, target) != 0 {
			// Never explicitly free a link container. Even failure leaves its
			// storage to the collector; no failed handle is published.
			return none
		}
		return GCWeakReference{ cell: cell }
	} $else {
		return none
	}
}

// get returns a strong pointer to the live target, or nil for an empty/expired
// handle. The returned pointer must remain strongly reachable while used.
// Reading under the collector lock prevents recreating a strong pointer between
// the collector's reclamation decision and clearing the disappearing link.
pub fn (reference GCWeakReference) get() voidptr {
	$if gcboehm ?&& !gcboehm_leak ? {
		if reference.cell == unsafe { nil } { return unsafe { nil } }
		return C.GC_call_with_alloc_lock(gc_weak_load, reference.cell)
	} $else {
		return unsafe { nil }
	}
}

fn gc_weak_read_find_leak(data voidptr) voidptr {
	$if gcboehm ? {
		unsafe { *(&bool(data)) = C.GC_get_find_leak() != 0 }
	}
	return unsafe { nil }
}

fn gc_weak_load(data voidptr) voidptr {
	return unsafe { (&GCWeakCell(data)).target }
}
