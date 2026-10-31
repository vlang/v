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
	// The fields below are only filled by RUSAGE_INFO_V4 and later.
	ri_child_user_time               u64
	ri_child_system_time             u64
	ri_child_pkg_idle_wkups          u64
	ri_child_interrupt_wkups         u64
	ri_child_pageins                 u64
	ri_child_elapsed_abstime         u64
	ri_diskio_bytesread              u64
	ri_diskio_byteswritten           u64
	ri_cpu_time_qos_default          u64
	ri_cpu_time_qos_maintenance      u64
	ri_cpu_time_qos_background       u64
	ri_cpu_time_qos_utility          u64
	ri_cpu_time_qos_legacy           u64
	ri_cpu_time_qos_user_initiated   u64
	ri_cpu_time_qos_user_interactive u64
	ri_billed_system_time            u64
	ri_serviced_system_time          u64
	ri_logical_writes                u64
	ri_lifetime_max_phys_footprint   u64
	ri_instructions                  u64
	ri_cycles                        u64
	ri_billed_energy                 u64
	ri_serviced_energy               u64
	ri_interval_max_phys_footprint   u64
	ri_runnable_time                 u64
}

@[c_extern]
fn C.proc_pid_rusage(pid int, flavor int, usage &V3RusageInfo) int

fn current_limit_memory() LimitMemory {
	$if macos || ios {
		mut usage := V3RusageInfo{}
		if C.proc_pid_rusage(os.getpid(), 0, &usage) == 0 {
			return LimitMemory{
				kb:     i64(usage.ri_phys_footprint / 1024)
				metric: 'physical footprint'
			}
		}
	}
	return LimitMemory{
		kb:     current_rss_kb()
		metric: 'RSS'
	}
}
