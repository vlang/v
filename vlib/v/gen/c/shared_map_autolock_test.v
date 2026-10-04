module c

import os

const shared_map_autolock_program = 'module main

fn range_bounds(shared m map[string]int) int {
	mut n := 0
	for i in m["start"] .. m["limit"] {
		n += i
	}
	for i in 0 .. m.len {
		n += i
	}
	return n
}

fn select_operands(shared m map[string]int, shared chans map[string]chan int) int {
	ch := chan int{cap: 1}
	select {
		ch <- m["start"] {}
	}
	select {
		chans["c"] <- 5 {}
	}
	mut got := 0
	select {
		x := <-chans["c"] {
			got += x
		}
	}
	select {
		y := <-ch {
			got += y
		}
		m["limit"] * 1000000 {}
	}
	return got
}

fn main() {
	shared m := map[string]int{}
	m["start"] = 1
	m["limit"] = 4
	shared chans := map[string]chan int{}
	chans["c"] = chan int{cap: 1}
	println(range_bounds(shared m))
	println(select_operands(shared m, shared chans))
}
'

// shared_map_accesses_outside_locks returns the lines of the C function `name` that access
// the storage of a `shared` map without holding its lock, and the number of accesses.
fn shared_map_accesses_outside_locks(csrc string, name string) ([]string, int) {
	lines := csrc.split_into_lines()
	mut start := -1
	for i, line in lines {
		if line.contains(' ${name}(') && line.ends_with(') {') && !line.starts_with(' ')
			&& !line.starts_with('\t') {
			start = i + 1
			break
		}
	}
	assert start > 0, 'missing C function `${name}`'
	mut depth := 0
	mut accesses := 0
	mut unlocked := []string{}
	for line in lines[start..] {
		if line == '}' {
			break
		}
		if line.contains('sync__RwMutex__rlock(') || line.contains('sync__RwMutex__lock(') {
			depth++
			continue
		}
		if line.contains('sync__RwMutex__runlock(') || line.contains('sync__RwMutex__unlock(') {
			depth--
			continue
		}
		if line.contains('->val') && !line.contains('->mtx') {
			accesses++
			if depth <= 0 {
				unlocked << line.trim_space()
			}
		}
	}
	return unlocked, accesses
}

fn test_shared_map_reads_in_range_bounds_and_select_cases_are_locked() {
	root := os.join_path(os.vtmp_dir(), 'shared_map_autolock_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'program.v')
	os.write_file(source, shared_map_autolock_program)!
	csource := os.join_path(root, 'program.c')
	gen := os.exec([@VEXE, '-o', csource, source])
	assert gen.exit_code == 0, gen.output
	csrc := os.read_file(csource)!
	range_unlocked, range_accesses := shared_map_accesses_outside_locks(csrc, 'range_bounds')
	assert range_accesses >= 3
	assert range_unlocked == []
	select_unlocked, select_accesses := shared_map_accesses_outside_locks(csrc, 'select_operands')
	assert select_accesses >= 4
	assert select_unlocked == []
	executable := os.join_path(root, 'program')
	build := os.exec([@VEXE, '-o', executable, source])
	assert build.exit_code == 0, build.output
	run := os.exec([executable])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['7', '6']
}
