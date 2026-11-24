module profile

import math
import os
import strconv
import strings

$if macos && amd64 && !native {
	// Report owns a report file, or borrows standard output. Its lifetime also
	// suspends profiling so formatting and I/O do not change the sampled counters.
	pub struct Report {
	mut:
		output           os.File
		owned            bool
		available        bool
		finished         bool
		previous_enabled bool
	}

	// begin_report opens path, or borrows stdout for "-". An unavailable report
	// silently discards rows, matching the compiler's existing reporting policy.
	@[markused; no_profile; no_trace]
	pub fn begin_report(path string) Report {
		previous := v__profile_enabled
		v__profile_enabled = false
		if path == '-' {
			return Report{
				output:           os.stdout()
				available:        true
				previous_enabled: previous
			}
		}
		output := os.create(path) or {
			return Report{ previous_enabled: previous }
		}
		return Report{
			output:           output
			owned:            true
			available:        true
			previous_enabled: previous
		}
	}

	// write_report_row writes the existing profile columns, widths and units.
	// Raw reports use V's standard fixed formatting: exact decimal ties round
	// away from zero, while hosted C stdio normally rounds ties to even.
	@[markused; no_profile; no_trace]
	pub fn write_report_row(mut report Report, calls u64, total_ns f64, self_ns f64, name string) {
		if !report.available || calls == 0 { return }
		mut line := strings.new_builder(128)
		// Repeated explicit reports must also release their rows without a GC.
		defer { unsafe { line.free() } }
		strconv.format_dec_sb(calls, strconv.BF_param{ len0: 14 }, mut line)
		line.write_u8(` `)
		append_report_float(mut line, total_ns / 1_000_000.0, 3)
		line.write_string('ms ')
		append_report_float(mut line, self_ns / 1_000_000.0, 3)
		line.write_string('ms ')
		append_report_float(mut line, total_ns / f64(calls), 0)
		line.write_string('ns ')
		line.write_string(name)
		line.write_string(' \n')
		// The writer borrows this complete row while its builder remains alive.
		unsafe {
			report.output.write_full_buffer(line.data, uint(line.len)) or { report.available = false }
		}
	}

	@[no_profile; no_trace]
	fn append_report_float(mut line strings.Builder, value f64, precision int) {
		// Use the existing exact formatter rather than an interpolation fast path:
		// large integral counters must keep every represented binary-value digit.
		strconv.format_fl_sb(value, strconv.BF_param{
			len0:     14
			len1:     precision
			positive: math.f64_bits(value) >> 63 == 0
		}, mut line)
	}

	// end_report closes an owned file and restores the preceding profiling state.
	// Standard output remains open, including when its descriptor is reused later.
	@[markused; no_profile; no_trace]
	pub fn end_report(mut report Report) {
		if report.finished { return }
		if report.owned { report.output.close() }
		report.available = false
		report.finished = true
		v__profile_enabled = report.previous_enabled
	}
}
