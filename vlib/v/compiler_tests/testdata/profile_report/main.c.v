@[has_globals]
module main

import os

#flag @DIR/format_oracle.c
#include "@DIR/format_oracle.h"

fn C.profile_report_expected(&u8, uint, u64, f64, f64, &u8) i32
fn C.profile_report_fd_is_open(i32) i32
fn C.profile_report_close(i32) i32

__global v__profile_enabled = true

struct Sample {
	calls u64
	total f64
	self  f64
}

fn require_report(ok bool, detail string) {
	if !ok {
		eprintln('profile report: ' + detail)
		exit(93)
	}
}

fn expected_row(calls u64, total f64, own f64, name string) string {
	mut buffer := [2048]u8{}
	size := C.profile_report_expected(&buffer[0], sizeof(buffer), calls, total, own, name.str)
	require_report(size >= 0 && size < buffer.len, 'C formatter bounds')
	return buffer[..int(size)].bytestr()
}

fn main() {
	path := os.args[1]
	mut report := begin_report(path)
	require_report(!v__profile_enabled, 'formatting disables profiling')
	fd := report.output.fd
	require_report(C.profile_report_fd_is_open(i32(fd)) == 1, 'report owns an open file')
	mut expected := ''
	for sample in [Sample{1, 0, 0}, Sample{3, 7654321, 2345678},
		Sample{u64(0xffffffffffffffff), 1e6, -0.0}, Sample{1, 1e23, -1e23}] {
		write_report_row(mut report, sample.calls, sample.total, sample.self, 'main__observed')
		expected += expected_row(sample.calls, sample.total, sample.self, 'main__observed')
	}
	// V's documented fixed-format ties round away from zero. The C oracle
	// independently checks the remaining layout, including the trailing space.
	write_report_row(mut report, 25000, 62500, -62500, 'main__tie')
	expected += expected_row(25000, 62500, -62500, 'main__tie').replace('0.062ms', '0.063ms').replace('             2ns', '             3ns')
	write_report_row(mut report, 0, 1, 1, 'must_not_appear')
	end_report(mut report)
	require_report(v__profile_enabled, 'report restores enabled state')
	require_report(C.profile_report_fd_is_open(i32(fd)) == 0, 'report closes its owned descriptor')
	actual := os.read_file(path) or { panic(err) }
	require_report(actual == expected, 'report schema and numeric output')
	v__profile_enabled = false
	end_report(mut report)
	require_report(!v__profile_enabled, 'finishing twice preserves the new state')

	mut failed := begin_report(os.join_path(path, 'not_a_directory'))
	write_report_row(mut failed, 1, 1, 1, 'ignored')
	end_report(mut failed)
	require_report(!v__profile_enabled, 'failed open restores disabled state')
	v__profile_enabled = true

	mut broken := begin_report(path)
	require_report(C.profile_report_close(i32(broken.output.fd)) == 0, 'close output to force a write failure')
	write_report_row(mut broken, 1, 1, 1, 'ignored')
	end_report(mut broken)
	require_report(v__profile_enabled, 'failed write restores enabled state')

	mut borrowed := begin_report('-')
	write_report_row(mut borrowed, 1, 0, 0, 'main__stdout')
	end_report(mut borrowed)
	require_report(v__profile_enabled && C.profile_report_fd_is_open(1) == 1, 'standard output stays open')
	println('V profile report writer passed')
}
