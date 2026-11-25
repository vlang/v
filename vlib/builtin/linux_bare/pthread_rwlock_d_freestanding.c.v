module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_pthread_abi.h"
	#include <errno.h>
	fn C.v_rwlock_size() uint
	fn C.v_rwlock_attributes_size() uint

	struct BarePthreadRwlock {
	mut:
		state      u32
		writers    u32
		sequence   u32
		attributes u32
	}

	@[export: 'v_rwlock_attr_init'; markused]
	fn bare_rw_attr_init(address uint) i32 {
		unsafe { vmemset(voidptr(address), 0, int(C.v_rwlock_attributes_size())) }
		return 0
	}
	@[export: 'v_rwlock_attr_destroy'; markused]
	fn bare_rw_attr_destroy(_ uint) i32 { return 0 }
	@[export: 'v_rwlock_attr_setshared'; markused]
	fn bare_rw_attr_setshared(address uint, sharing i32) i32 {
		if sharing != i32(C.PTHREAD_PROCESS_PRIVATE) && sharing != i32(C.PTHREAD_PROCESS_SHARED) {
			return i32(C.EINVAL)
		}
		unsafe {
			*(&u32(address)) = (*(&u32(address)) & ~u32(1)) | if sharing == i32(C.PTHREAD_PROCESS_SHARED) {
				u32(1)
			} else {
				u32(0)
			}
		}
		return 0
	}
	@[export: 'v_rwlock_attr_getshared'; markused]
	fn bare_rw_attr_getshared(address uint, output uint) i32 {
		value := if unsafe { *(&u32(address)) } & 1 != 0 {
			i32(C.PTHREAD_PROCESS_SHARED)
		} else {
			i32(C.PTHREAD_PROCESS_PRIVATE)
		}
		unsafe { *(&i32(output)) = value }
		return 0
	}
	@[export: 'v_rwlock_attr_setkind'; markused]
	fn bare_rw_attr_setkind(address uint, kind i32) i32 {
		if kind < 0 || kind > 2 { return i32(C.EINVAL) }
		unsafe { *(&u32(address)) = (*(&u32(address)) & 1) | (u32(kind) << 1) }
		return 0
	}
	@[export: 'v_rwlock_attr_getkind'; markused]
	fn bare_rw_attr_getkind(address uint, output uint) i32 {
		unsafe { *(&i32(output)) = i32(*(&u32(address)) >> 1) }
		return 0
	}
	@[export: 'v_rwlock_init'; markused]
	fn bare_rw_init(address uint, attributes uint) i32 {
		unsafe { vmemset(voidptr(address), 0, int(C.v_rwlock_size())) }
		mut rw := unsafe { &BarePthreadRwlock(address) }
		if attributes != 0 { rw.attributes = unsafe { *(&u32(attributes)) } }
		return 0
	}
	@[export: 'v_rwlock_destroy'; markused]
	fn bare_rw_destroy(address uint) i32 {
		mut rw := unsafe { &BarePthreadRwlock(address) }
		if bare_load_u32(&rw.state) != 0 || bare_load_u32(&rw.writers) != 0 { return i32(C.EBUSY) }
		rw.attributes |= 256
		return 0
	}
	fn bare_rw_wake(mut rw BarePthreadRwlock) {
		mut previous := bare_load_u32(&rw.sequence)
		for !bare_cas_u32(&rw.sequence, previous, previous + 1) {
			previous = bare_load_u32(&rw.sequence)
		}
		operation := if rw.attributes & 1 != 0 { 1 } else { 129 }
		bf_futex_raw(&rw.sequence, operation, 0x7fffffff, unsafe { nil }, unsafe { nil }, 0)
	}
	fn bare_rw_add_writer(mut rw BarePthreadRwlock, added bool) {
		mut previous := bare_load_u32(&rw.writers)
		for {
			next := if added { previous + 1 } else { previous - 1 }
			if bare_cas_u32(&rw.writers, previous, next) { return }
			previous = bare_load_u32(&rw.writers)
		}
	}
	@[export: 'v_rwlock_acquire'; markused]
	fn bare_rw_acquire(address uint, write i32, nonblocking i32, deadline uint) i32 {
		mut rw := unsafe { &BarePthreadRwlock(address) }
		if rw.attributes & 256 != 0 { return i32(C.EINVAL) }
		if write != 0 { bare_rw_add_writer(mut rw, true) }
		mut result := i32(0)
		for {
			sequence := bare_load_u32(&rw.sequence)
			state := bare_load_u32(&rw.state)
			// GNU's nonrecursive writer preference is an explicit attribute;
			// the default continues to admit recursive readers.
			available := if write != 0 {
				state == 0
			} else {
				state != 0xffffffff && ((rw.attributes >> 1) != 2 || bare_load_u32(&rw.writers) == 0)
			}
			if available {
				if write == 0 && state == 0x7fffffff {
					result = i32(C.EAGAIN)
					break
				}
				next := if write != 0 { u32(0xffffffff) } else { state + 1 }
				if bare_cas_u32(&rw.state, state, next) { break }
				continue
			}
			if nonblocking != 0 {
				result = i32(C.EBUSY)
				break
			}
			if deadline != 0 {
				timeout := unsafe { &i64(deadline) }
				if unsafe { timeout[1] } < 0 || unsafe { timeout[1] } >= 1_000_000_000 {
					result = i32(C.EINVAL)
					break
				}
				if unsafe { timeout[0] } < 0 {
					result = i32(C.ETIMEDOUT)
					break
				}
			}
			mut operation := if rw.attributes & 1 != 0 { 0 } else { 128 }
			if deadline != 0 { operation |= 9 | 256 }
			wait := bf_futex_raw(&rw.sequence, operation, sequence, unsafe { voidptr(deadline) }, unsafe { nil }, 0xffffffff)
			if wait < 0 && wait != -i64(C.EINTR) && wait != -i64(C.EAGAIN) {
				result = i32(-wait)
				break
			}
		}
		if write != 0 {
			bare_rw_add_writer(mut rw, false)
			if result != 0 { bare_rw_wake(mut rw) }
		}
		return result
	}
	@[export: 'v_rwlock_unlock'; markused]
	fn bare_rw_unlock(address uint) i32 {
		mut rw := unsafe { &BarePthreadRwlock(address) }
		if rw.attributes & 256 != 0 { return i32(C.EINVAL) }
		mut previous := bare_load_u32(&rw.state)
		for {
			if previous == 0 { return i32(C.EPERM) }
			next := if previous == 0xffffffff { u32(0) } else { previous - 1 }
			if bare_cas_u32(&rw.state, previous, next) { break }
			previous = bare_load_u32(&rw.state)
		}
		if previous == 0xffffffff || previous == 1 { bare_rw_wake(mut rw) }
		return 0
	}
}
