module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include <sys/resource.h>

	fn bare_scheduler_affinity_raw(pid i32, size uint, mask uint, setting bool) i64 {
		result := i64(sys_call3(if setting {
			u64(C.SYS_sched_setaffinity)
		} else {
			u64(C.SYS_sched_getaffinity)
		}, u64(pid), u64(size), u64(mask)))
		if result < 0 { return result }
		if !setting {
			for index := uint(result); index < size; index++ {
				unsafe { (&u8(mask))[index] = 0 }
			}
		}
		return 0
	}
	@[export: 'v_native_scheduler'; markused]
	fn bare_native_scheduler(operation i32, first i32, second i32, parameters uint, size uint) i64 {
		result := match operation {
			0 { sys_call1(u64(C.SYS_sched_getscheduler), u64(first)) }
			1 { sys_call3(u64(C.SYS_sched_setscheduler), u64(first), u64(second), u64(parameters)) }
			2 { sys_call2(u64(C.SYS_sched_getparam), u64(first), u64(parameters)) }
			3 { sys_call2(u64(C.SYS_sched_setparam), u64(first), u64(parameters)) }
			4 { sys_call1(u64(C.SYS_sched_get_priority_min), u64(first)) }
			5 { sys_call1(u64(C.SYS_sched_get_priority_max), u64(first)) }
			6 { sys_call2(u64(C.SYS_sched_rr_get_interval), u64(first), u64(parameters)) }
			7 { u64(bare_scheduler_affinity_raw(first, size, parameters, false)) }
			8 { u64(bare_scheduler_affinity_raw(first, size, parameters, true)) }
			else { u64(-i64(C.EINVAL)) }
		}
		return bare_native_result(result)
	}
	@[export: 'v_native_priority'; markused]
	fn bare_native_priority(which i32, who u32, priority i32, setting i32) i32 {
		if setting != 0 {
			return i32(bare_native_result(sys_call3(u64(C.SYS_setpriority), u64(which), u64(who), u64(priority))))
		}
		result := i64(sys_call2(u64(C.SYS_getpriority), u64(which), u64(who)))
		if result < 0 { return i32(bare_native_result(u64(result))) }
		// Linux returns 20-nice so successful negative priorities cannot be
		// confused with negative kernel errors. POSIX returns the nice value.
		return i32(20 - result)
	}
	@[export: 'v_native_nice'; markused]
	fn bare_native_nice(increment i32) i32 {
		current := i64(sys_call2(u64(C.SYS_getpriority), u64(C.PRIO_PROCESS), 0))
		if current < 0 { return i32(bare_native_result(u64(current))) }
		requested := i64(20) - current + i64(increment)
		priority := if requested < -20 {
			i64(-20)
		} else if requested > 19 {
			i64(19)
		} else {
			requested
		}
		result := i64(sys_call3(u64(C.SYS_setpriority), u64(C.PRIO_PROCESS), 0, u64(priority)))
		if result < 0 {
			bf_native_errno_cell = if result == -i64(C.EACCES) {
				i32(C.EPERM)
			} else {
				i32(-result)
			}
			return -1
		}
		return i32(priority)
	}
}
