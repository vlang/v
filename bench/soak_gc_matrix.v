// soak_gc_matrix.v - S0 soak matrix driver (tracker vlang/v#32525).
//
// Compiles bench/soak_gc.v once per mode and runs the cross-product:
//   v run bench/soak_gc_matrix.v
//   v run bench/soak_gc_matrix.v --asan   // opt-in ASan arm, BLOCKED where unsupported
//
// Matrix (S0 scope; tree 12..20 is the expensive end, run when quiet):
//   modes: vgc | vgc+gt (-d vgc_greentea) | vgc+par (-d vgc_parallel_mark)
//   scenarios: tree depth 12..20 | mutate depth 13+16 | threads depth 12 | fork depth 12
// Each cell prints PASS / FAIL / BLOCKED. Env/toolchain limits (fork on
// Windows, ASan on tcc) are BLOCKED, never FAIL.
module main

import os

struct SoakMode {
	name string
	defs []string
	bin  string
mut:
	built bool
}

struct SoakCase {
	mode     string
	scenario string
	depth    int
	rounds   int
	threads  int
	seed     u64
}

fn run_one(bin string, c SoakCase) string {
	args := [bin, '--scenario', c.scenario, '--depth', c.depth.str(), '--rounds', c.rounds.str(),
		'--threads', c.threads.str(), '--seed', c.seed.str()]
	r := os.exec(args)
	out := r.output.trim_space()
	// The harness prints SCENARIO first, then PASS. Keep the last line.
	mut last := ''
	for line in out.split_into_lines() {
		if line.trim_space() != '' {
			last = line
		}
	}
	if r.exit_code == 0 && last.starts_with('PASS') {
		return 'PASS'
	}
	if r.exit_code == 0 && last.starts_with('BLOCKED') {
		return 'BLOCKED'
	}
	eprintln('--- FAIL detail [${c.mode} ${c.scenario} depth=${c.depth}] exit=${r.exit_code}')
	eprintln(out)
	return 'FAIL'
}

fn main() {
	v_exe := os.getenv_opt('VEXE') or { @VEXE }
	src := os.join_path(@VMODROOT, 'bench', 'soak_gc.v')
	tmp := os.temp_dir()
	want_asan := '--asan' in os.args

	mut modes := [
		SoakMode{
			name: 'vgc'
			defs: []string{}
			bin:  os.join_path(tmp, 'soak_matrix_vgc')
		},
		SoakMode{
			name: 'vgc+gt'
			defs: ['-d', 'vgc_greentea']
			bin:  os.join_path(tmp, 'soak_matrix_gt')
		},
		SoakMode{
			name: 'vgc+par'
			defs: ['-d', 'vgc_parallel_mark']
			bin:  os.join_path(tmp, 'soak_matrix_par')
		},
	]

	// Compile each mode once.
	mut build_fail_n := 0
	mut unbuilt := []string{}
	for i in 0 .. modes.len {
		mut cmd := [v_exe, '-gc', 'vgc']
		for d in modes[i].defs {
			cmd << d
		}
		cmd << '-prod'
		cmd << '-o'
		cmd << modes[i].bin
		cmd << src
		print('compiling ${modes[i].name}...')
		flush_stdout()
		r := os.exec(cmd)
		if r.exit_code != 0 {
			eprintln(' FAILED')
			eprintln(r.output)
			modes[i].built = false
			build_fail_n++
			unbuilt << modes[i].name
			continue
		}
		modes[i].built = true
		println(' ok')
	}

	mut cases := []SoakCase{}
	for m in modes {
		if !m.built {
			continue
		}
		for depth in 12 .. 21 {
			cases << SoakCase{
				mode:     m.name
				scenario: 'tree'
				depth:    depth
				rounds:   2
				threads:  4
				seed:     42
			}
		}
		for mdepth in [13, 16] {
			cases << SoakCase{
				mode:     m.name
				scenario: 'mutate'
				depth:    mdepth
				rounds:   3
				threads:  4
				seed:     42
			}
		}
		cases << SoakCase{
			mode:     m.name
			scenario: 'threads'
			depth:    12
			rounds:   1
			threads:  4
			seed:     42
		}
		cases << SoakCase{
			mode:     m.name
			scenario: 'fork'
			depth:    12
			rounds:   1
			threads:  0
			seed:     42
		}
	}

	println('')
	println('mode\tscenario\tdepth\trounds\tthreads\tseed\tresult')
	mut pass_n := 0
	mut fail_n := 0
	mut blocked_n := 0
	for c in cases {
		mut bin := ''
		for m in modes {
			if m.name == c.mode {
				bin = m.bin
			}
		}
		res := run_one(bin, c)
		if res == 'PASS' {
			pass_n++
		} else if res == 'BLOCKED' {
			blocked_n++
		} else {
			fail_n++
		}
		println('${c.mode}\t${c.scenario}\t${c.depth}\t${c.rounds}\t${c.threads}\t${c.seed}\t${res}')
	}
	// Every mode that failed to build is a row of its own, so no failure is
	// silent when every build fails and no cases run at all.
	for name in unbuilt {
		println('${name}\t-\t-\t-\t-\t-\tBUILD_FAIL')
	}

	if want_asan {
		// The sanitizer arm is not implemented yet: it attempts no build, so it
		// is UNIMPLEMENTED rather than BLOCKED.
		println('asan\tUNIMPLEMENTED\trequires gcc/clang with -fsanitize=address; no sanitizer arm runs yet')
	} else {
		println('(asan arm not implemented; pass --asan to record it as UNIMPLEMENTED)')
	}

	println('')
	if unbuilt.len > 0 {
		println('unbuilt modes: ' + unbuilt.join(', '))
	}
	println('soak matrix: ${pass_n} PASS, ${fail_n} FAIL, ${blocked_n} BLOCKED, ${build_fail_n} BUILD_FAIL over ${cases.len} cases')
	if fail_n > 0 || build_fail_n > 0 {
		exit(1)
	}
}
