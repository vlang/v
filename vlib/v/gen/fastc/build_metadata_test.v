module fastc

import os
import v.cmdexec
import v.pref

fn test_build_metadata_keeps_shared_declarations_stable() ! {
	root := os.join_path(os.vtmp_dir(), 'fastc_metadata_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, r'module main
const stamp = @BUILD_TIMESTAMP
const date = @BUILD_DATE
const clock = @BUILD_TIME
const copied = stamp
const literal = "@BUILD_TIMESTAMP"
fn main() {
    println(copied)
    println(date)
    println(clock)
    println(literal)
}
')!
	base := pref.new_preferences()
	mut heads := []string{}
	mut initializers := []string{}
	for stamp in ['100', '200'] {
		prefs := pref.Preferences{
			...base
			building_v:      true
			build_timestamp: stamp
			build_date:      'date_${stamp}'
			build_time:      'time_${stamp}'
		}
		generated := generate_files_with_source_paths([source], &prefs)!
		heads << generated.c_pieces[..generated.units.head_end].join('')
		initializers << generated.c_pieces[generated.units.head_end..generated.units.solo_end].join('')
		assert heads.last().contains('#define main__literal ')
		assert !heads.last().contains('#define main__stamp ')
		assert !heads.last().contains('date_${stamp}')
		assert initializers.last().contains('date_${stamp}')
	}
	assert heads[0] == heads[1]
	assert initializers[0] != initializers[1]
}

fn test_build_metadata_is_initialized_before_dependents() ! {
	root := os.join_path(os.vtmp_dir(), 'fastc_metadata_init_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'metadata'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'metadata_test' }")!
	os.write_file(os.join_path(root, 'metadata', 'value.v'), r'module metadata
pub const stamp = @BUILD_TIMESTAMP
pub const date = @BUILD_DATE
pub const clock = @BUILD_TIME
pub const copied = stamp
pub const parsed = stamp.i64()
struct Snapshot { captured string = stamp }
pub const saved = Snapshot{}
__global initialized = copied
__global snapshot = Snapshot{}
fn init() {
    assert copied == stamp && initialized == stamp
    assert saved.captured == stamp && snapshot.captured == stamp
    assert parsed > 0 && date.len > 0 && clock.len > 0
}
')!
	os.write_file(os.join_path(root, 'main.v'), r'module main
import metadata
fn main() {
    assert metadata.copied == metadata.stamp
    assert metadata.parsed == metadata.stamp.i64()
    assert metadata.stamp == @BUILD_TIMESTAMP
    assert metadata.date == @BUILD_DATE
    assert metadata.clock == @BUILD_TIME
}
')!
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
