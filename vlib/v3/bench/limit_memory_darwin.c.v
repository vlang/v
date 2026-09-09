module bench

import os

struct V3RusageInfo {
	ri_uuid               [16]u8
	ri_user_time          u64
	ri_system_time        u64
	ri_pkg_idle_wkups     u64
	ri_interrupt_wkups    u64
	ri_pageins            u64
	ri_wired_size         u64
	ri_resident_size      u64
	ri_phys_footprint     u64
	ri_proc_start_abstime u64
	ri_proc_exit_abstime  u64
}

@[c_extern]
fn C.proc_pid_rusage(pid int, flavor int, usage &V3RusageInfo) int

fn current_limit_memory() LimitMemory {
	mut usage := V3RusageInfo{}
	if C.proc_pid_rusage(os.getpid(), 0, &usage) == 0 {
		return LimitMemory{
			kb:     i64(usage.ri_phys_footprint / 1024)
			metric: 'physical footprint'
		}
	}
	return LimitMemory{
		kb:     current_rss_kb()
		metric: 'RSS'
	}
}
