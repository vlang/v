module fastc

import os
import v.cmdexec

// Only an `or` block's own final `if`/`match` is its value. One that ends a block
// nested inside the `or` block (a `for` or `if` body) is an ordinary statement of
// that block; it was hoisted out of its scope (an undeclared C variable) or dropped.
fn test_fastc_or_block_nested_final_if_and_match_stay_statements() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_or_block_nested_final_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main
struct Res {
	data     []u8
	consumed int
}
fn maybe(x int) ?int {
	if x > 2 { return x * 10 }
	return none
}
fn fails(x int) !int {
	if x < 0 { return error("neg") }
	return error("always")
}
fn decode(data []u8) ![]u8 {
	if data.len > 3 { return error("trailing") }
	if data.len < 3 { return error("truncated") }
	return data.clone()
}
fn leading(data []u8) !Res {
	whole := decode(data) or {
		if !err.msg().contains("trailing") { return err }
		mut low := 1
		mut high := data.len - 1
		for low <= high {
			middle := low + (high - low) / 2
			if decoded := decode(data[..middle]) {
				return Res{decoded, middle}
			} else {
				if err.msg().contains("trailing") { high = middle - 1 } else { low = middle + 1 }
			}
		}
		return error("not found")
	}
	return Res{whole, data.len}
}
fn final_if_guard(x int) int {
	return fails(x) or {
		if v := maybe(x) { v } else { -1 }
	}
}
fn final_match(x int) int {
	return fails(x) or {
		match x {
			1 { 100 }
			else { 200 }
		}
	}
}
fn nested_for(x int) int {
	return fails(x) or {
		mut found := 0
		for i in 0 .. 5 {
			step := i + x
			if v := maybe(step) { found += v } else { found += step }
		}
		found
	}
}
fn nested_if(x int) int {
	return fails(x) or {
		mut total := 1
		if x > 0 {
			limit := x * 2
			if v := maybe(limit) { total = v } else { total = limit }
		}
		total
	}
}
fn nested_match(x int) int {
	return fails(x) or {
		mut acc := 0
		for i in 0 .. 3 {
			k := i + x
			match k {
				2 { acc += 50 }
				else { acc += k }
			}
		}
		acc
	}
}
fn main() {
	r := leading([u8(1), 2, 3, 4, 5, 6]) or { panic(err) }
	println("\${r.data} \${r.consumed}")
	for x in [1, 2, 3] {
		println("\${final_if_guard(x)} \${final_match(x)} \${nested_for(x)} \${nested_if(x)} \${nested_match(x)}")
	}
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '[1, 2, 3] 3\n-1 100 123 2 54\n-1 200 182 40 57\n30 200 250 60 12', run.output
}
