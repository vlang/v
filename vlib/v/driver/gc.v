module driver

import v.pref

const v3_default_gc_mode = 'boehm_full_opt'

fn v3_gc_mode_defines(gc_mode string, disable_gc bool) ![]string {
	mode := if gc_mode in ['', 'boehm'] { v3_default_gc_mode } else { gc_mode }
	defines := match mode {
		'none' { []string{} }
		'boehm_full' { ['gcboehm', 'gcboehm_full'] }
		'boehm_incr' { ['gcboehm', 'gcboehm_incr'] }
		'boehm_full_opt' { ['gcboehm', 'gcboehm_full', 'gcboehm_opt'] }
		'boehm_incr_opt' { ['gcboehm', 'gcboehm_incr', 'gcboehm_opt'] }
		'boehm_leak' { ['gcboehm', 'gcboehm_leak'] }
		'vgc' { ['vgc'] }
		else {
			return error('unknown garbage collection mode `-gc ${gc_mode}`; supported modes are: boehm, boehm_full, boehm_incr, boehm_full_opt, boehm_incr_opt, boehm_leak, none, vgc')
		}
	}
	if disable_gc {
		return []string{}
	}
	return defines
}

fn v3_gc_is_disabled(building_v bool, cross_output bool, target_os string, target_arch string) bool {
	if building_v || cross_output {
		return true
	}
	host := pref.host_target()
	return pref.normalized_os(target_os.trim_space().to_lower()) != host.os
		|| pref.normalized_arch(target_arch.trim_space().to_lower()) != host.arch
}

fn apply_v3_gc_mode(gc_mode string, disable_gc bool, mut user_defines []string, mut compile_values map[string]string) ! {
	for define in v3_gc_mode_defines(gc_mode, disable_gc)! {
		record_user_define(mut user_defines, mut compile_values, define)
	}
}
