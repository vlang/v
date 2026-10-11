module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_labeled_break_continue_and_goto_before_and_after_transform() {
	$if macos && arm64 {
		source := r'module main
fn C.exit(int)
fn C.alarm(u32) u32
fn main() {
    C.alarm(5)
    outer_break: for {
        for _ in 0 .. 2 { break outer_break }
        C.exit(1)
    }
    mut total := 0
    outer_continue: for i in 0 .. 3 {
        for _ in 0 .. 2 {
            total += i + 1
            continue outer_continue
        }
        C.exit(2)
    }
    if total != 6 { C.exit(3) }
    mut i := 0
    mut j := 10
    post_loop: for ; i < 3; i, j = i + 1, j + 2 {
        for _ in 0 .. 1 { continue post_loop }
        C.exit(4)
    }
    if i != 3 || j != 16 { C.exit(5) }
    mut k := 0
    mut p := 0
    for ; k < 3; k, p = k + 1, p + 2 {
        if k >= 0 { continue }
        C.exit(6)
    }
    if k != 3 || p != 6 { C.exit(7) }
    mut goto_count := 0
    repeat:
    goto_count++
    if goto_count < 2 { unsafe { goto repeat } }
    if goto_count != 2 { C.exit(8) }
    C.alarm(0)
}
'
		for transformed in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_loop_labels_${os.getpid()}_${transformed}.v')
			output := path.all_before_last('.')
			os.write_file(path, source)!
			defer {
				os.rm(path) or {}
				os.rm(output) or {}
			}
			mut preferences := pref.new_preferences()
			preferences.backend = 'arm64'
			mut p := parser.Parser.new(preferences)
			mut a := p.parse_file(path)
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := types.TypeChecker.new(a)
			tc.collect(a)
			tc.annotate_types()
			assert tc.errors.len == 0, tc.errors.str()
			if transformed {
				transform.transform(mut a, tc)
			}
			m := ssa.build_with_used(a, map[string]bool{}, tc)
			mut g := Gen.new(m)
			g.gen()
			g.write_and_link(output)
			result := os.exec([output])
			assert result.exit_code == 0, 'transformed=${transformed}, exit ${result.exit_code}: ${result.output}'
		}
	}
}
