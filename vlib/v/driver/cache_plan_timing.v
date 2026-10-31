module driver

import os
import time

// V3_CACHE_TIMING=1 makes the module cache print, once per build, where
// prepare_v3_module_cache and compile_v3_cached_object spent their time: the
// microseconds of each step summed over the modules, the number of times the step
// ran, and (for the declaration header) the bytes that went in and the bytes a
// module object was given. The steps do not overlap, so their total is the stage.

// V3PlanStep names one step of the module plan.
enum V3PlanStep {
	split
	closure_dependencies
	main_prefix
	builtin_bundle
	hash_body
	dependency_signatures
	reusable_check
	declaration_header
	header_subset
	native_prototypes
	native_inputs
	module_source
	prune_static_strings
	pooled_signature
	write_source
	c_compiler
	publish_object
	scope_free
	publish_cache_entry
}

const v3_plan_step_count = 19

// V3PlanTiming holds the sums of a build. The slots are fixed arrays, not maps: the
// plan runs inside prealloc scopes, which a map filled from inside one does not survive.
struct V3PlanTiming {
mut:
	micros       [19]i64
	calls        [19]int
	bytes_in     i64
	bytes_given  i64
	subset_calls int
}

const v3_plan_timing = &V3PlanTiming{}

// V3PlanClock is the stopwatch of one function; it does nothing unless V3_CACHE_TIMING is set.
struct V3PlanClock {
	on bool
mut:
	sw time.StopWatch
}

fn new_v3_plan_clock() V3PlanClock {
	return V3PlanClock{
		on: os.getenv('V3_CACHE_TIMING') != ''
		sw: time.new_stopwatch()
	}
}

// restart drops the time since the last lap: what ran in between is counted elsewhere.
fn (mut c V3PlanClock) restart() {
	if c.on {
		c.sw.restart()
	}
}

// lap adds the time since the last lap to `step`.
fn (mut c V3PlanClock) lap(step V3PlanStep) {
	if !c.on {
		return
	}
	us := c.sw.elapsed().microseconds()
	c.sw.restart()
	unsafe {
		mut t := &V3PlanTiming(v3_plan_timing)
		t.micros[int(step)] += us
		t.calls[int(step)]++
	}
}

// note_header_subset records the bytes of a declaration header and of what a module was given.
fn (c &V3PlanClock) note_header_subset(bytes_in int, bytes_given int) {
	if !c.on {
		return
	}
	unsafe {
		mut t := &V3PlanTiming(v3_plan_timing)
		t.bytes_in += bytes_in
		t.bytes_given += bytes_given
		t.subset_calls++
	}
}

fn v3_plan_timing_report() string {
	mut out := 'V3 plan timing (us, calls):'
	mut total := i64(0)
	for i in 0 .. v3_plan_step_count {
		step := unsafe { V3PlanStep(i) }
		out += '\n  ${step.str():-22} ${v3_plan_timing.micros[i]:10} ${v3_plan_timing.calls[i]:4}'
		total += v3_plan_timing.micros[i]
	}
	out += '\n  ${'total':-22} ${total:10}'
	if v3_plan_timing.subset_calls > 0 {
		out += '\n  declaration bytes: ${v3_plan_timing.bytes_in} offered, ${v3_plan_timing.bytes_given} given, over ${v3_plan_timing.subset_calls} module object(s)'
	}
	return out
}
