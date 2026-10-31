module fastc

import os
import v.cmdexec

fn test_fastc_channels_use_the_sync_runtime() {
	$if !macos && !linux {
		return
	}
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	root := os.join_path(os.vtmp_dir(), 'fastc_channel_runtime_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Selection {
	items chan []int
}
fn publish(items chan []int) {
	items <- [3, 5, 8]
}
fn finish(channel chan bool) { channel <- true }
fn receive(selection Selection) []int {
	mut values := []int{}
	values = <-selection.items
	return values
}
fn main() {
	big := int(1) << 40
	numbers := chan int{cap: 2}
	select {
		numbers <- big {}
		else { panic("buffered send was not ready") }
	}
	assert numbers.cap == 2
	offsets := [7]
	assert numbers.len + offsets[0] == 8
	assert numbers.len == 1
	value := <-numbers
	assert value == big
	mut empty := false
	select {
		_ := <-numbers { panic("empty receive was ready") }
		else { empty = true }
	}
	assert empty
	numbers <- big + 1
	numbers.close()
	assert numbers.closed
	mut count := 0
	for {
		received := <-numbers or {
			assert err.msg() == "channel closed"
			break
		}
		assert received == big + 1
		count++
	}
	assert count == 1
	selection := Selection{items: chan []int{}}
	thread := spawn publish(selection.items)
	values := receive(selection)
	assert values == [3, 5, 8]
	thread.wait()
	selection.items.close()
	done := chan bool{}
	writer := spawn finish(done)
	select {
		ready := <-done { assert ready }
	}
	writer.wait()
	println("channels work")
}
') or { panic(err) }
	fastc_channel_runtime_build_run(root, vroot, 'channels work')
}

@[unsafe]
fn test_fastc_compiler_workers_run_parallel_batches() {
	$if !macos && !linux {
		return
	}
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	root := os.join_path(os.vtmp_dir(), 'fastc_worker_runtime_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import v.workers
struct Job { mut: value int }
fn do_job(arg voidptr) voidptr {
	mut job := unsafe { &Job(arg) }
	job.value++
	return unsafe { nil }
}
fn main() {
	mut pool := workers.new(2)
	mut jobs := []&Job{}
	mut tasks := []workers.Task{}
	for _ in 0 .. 64 {
		job := &Job{}
		jobs << job
		tasks << workers.Task{run: do_job, arg: voidptr(job)}
	}
	assert pool.run(tasks)
	assert pool.run(tasks)
	assert pool.tasks_run() == 128
	for job in jobs { assert job.value == 2 }
	pool.close()
	println("workers work")
}
') or { panic(err) }
	fastc_channel_runtime_build_run(root, vroot, 'workers work')
}

fn test_fastc_mutable_fixed_array_parameters_keep_element_types() {
	$if !macos && !linux {
		return
	}
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	root := os.join_path(os.vtmp_dir(), 'fastc_mut_fixed_arrays_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Entry {
	text string
mut:
	count u16
}
fn populate(mut texts [4]string, mut numbers [4]u16, mut entries [4]Entry, mut pointers [4]&Entry) {
	texts[1] = "hello"
	assert texts[1].len == 5
	numbers[1] = u16(513)
	entries[1] = Entry{text: texts[1], count: numbers[1]}
	pointers[1] = &entries[1]
	pointers[1].count += 2
}
fn main() {
	mut texts := [4]string{}
	mut numbers := [4]u16{}
	mut entries := [4]Entry{}
	mut pointers := [4]&Entry{}
	populate(mut texts, mut numbers, mut entries, mut pointers)
	assert texts[1] == "hello"
	assert numbers[1] == 513
	assert entries[1].count == 515
	assert pointers[1].text == "hello"
	println("fixed arrays work")
}
') or { panic(err) }
	fastc_channel_runtime_build_run(root, vroot, 'fixed arrays work')
}

fn fastc_channel_runtime_build_run(root string, vroot string, expected string) {
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], vroot, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, '${root}: ${build.output}'
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == expected, run.output
}
