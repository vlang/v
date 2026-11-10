module fastc

import v.pref

// A self-host build skips the bodies of functions that nothing calls, so the
// only `spawn` of a thread handle type can disappear while the struct that
// embeds the handle is still declared. `v self -b fastc` hit this with
// `FastcNativeCompile.worker`, a `thread os.Result` spawned only by v.driver.
fn test_selfhost_struct_thread_field_without_reachable_spawn_declares_handle() {
	$if windows {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Answer {
	value int
}

struct Job {
mut:
	worker thread Answer
	active bool
}

fn compute() Answer {
	return Answer{
		value: 42
	}
}

fn start() Job {
	return Job{
		worker: spawn compute()
		active: true
	}
}

fn main() {
	job := Job{}
	println(job.active)
}
',
		'thread_field_typedef.v', prefs) or { panic(err) }
	assert !c_source.contains('__vf_thread_wait_'), c_source
	thread_type := c_source.all_after('struct Job {').all_before(' worker;').trim_space()
	assert thread_type.starts_with('__vf_thread_k'), c_source
	typedef := fastc_thread_typedef(thread_type)
	typedef_at := c_source.index(typedef) or { panic('missing `${typedef}`:\n${c_source}') }
	job_at := c_source.index('struct Job {') or { panic(c_source) }
	assert typedef_at < job_at, c_source
	assert c_source.count(typedef) == 1, c_source
}

// A reachable `spawn` registers the same typedef; the field must not add a second one.
fn test_struct_thread_field_with_spawn_declares_handle_once() {
	$if windows {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Job {
mut:
	worker thread int
}

fn compute() int {
	return 42
}

fn main() {
	mut job := Job{
		worker: spawn compute()
	}
	println(job.worker.wait())
}
',
		'thread_field_typedef_spawned.v', prefs) or { panic(err) }
	thread_type := c_source.all_after('struct Job {').all_before(' worker;').trim_space()
	assert thread_type.starts_with('__vf_thread_k'), c_source
	assert c_source.count(fastc_thread_typedef(thread_type)) == 1, c_source
}
