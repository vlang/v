// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module stagetimes

// StageMeasurements holds the time and peak RSS of each compiler stage, and the
// number of compiled V lines.
pub struct StageMeasurements {
pub mut:
	scan_ms      int
	parse_ms     int
	check_ms     int
	cgen_ms      int
	scan_rss_kb  int
	parse_rss_kb int
	check_rss_kb int
	cgen_rss_kb  int
	vlines       int
}

// parse_stage_measurements reads the stage timings printed by the old compiler
// (`-show-timings`) or by V3 (`-show-timings -stats`).
pub fn parse_stage_measurements(output string) !StageMeasurements {
	if output.contains(' MB RSS') {
		return parse_v3_stage_measurements(output)
	}

	mut result := StageMeasurements{
		vlines: parse_vlines(output)
	}
	lines := output.split_into_lines()
	if lines.len == 3 {
		result.parse_ms = lines[0].before('.').int()
		result.check_ms = lines[1].before('.').int()
		result.cgen_ms = lines[2].before('.').int()
	} else {
		ms_lines := lines.map(it.split('  ms '))
		for line in ms_lines {
			if line.len == 2 {
				match line[1] {
					'SCAN' { result.scan_ms = line[0].int() }
					'PARSE' { result.parse_ms = line[0].int() }
					'CHECK' { result.check_ms = line[0].int() }
					'C GEN' { result.cgen_ms = line[0].int() }
					else {}
				}
			}
		}
	}
	// Both output formats set parse/check/cgen on success; if none parsed, the
	// output was unusable (e.g. a crash that still exited 0), so reject it rather
	// than letting arrays.min pick these zeroes into the stored row.
	if result.parse_ms == 0 && result.check_ms == 0 && result.cgen_ms == 0 {
		return error('could not parse stage timings from output:\n${output}')
	}
	return result
}

fn parse_v3_stage_measurements(output string) !StageMeasurements {
	mut result := StageMeasurements{
		vlines: parse_vlines(output)
	}
	for raw_line in output.split_into_lines() {
		line := raw_line.trim_space()
		ms_pos := line.index(' ms') or { continue }
		left := line[..ms_pos].trim_space()
		parts := left.fields()
		if parts.len < 2 {
			continue
		}
		label := parts[..parts.len - 1].join(' ')
		ms := int(parts.last().f64())
		rss_mb := rss_mb_from_v3_line(line)
		match true {
			label == 'parse setup/cache' {
				// v3 scans while setting up its parse pipeline, so this is the
				// closest equivalent to the old compiler's standalone SCAN stage.
				result.scan_ms = ms
				result.scan_rss_kb = rss_mb * 1024
			}
			label == 'parse .vh' || label.starts_with('parse .v') || label == 'resolve imports' {
				result.parse_ms += ms
				result.parse_rss_kb = rss_mb * 1024
			}
			label == 'check' || label.starts_with('check (') {
				result.check_ms = ms
				result.check_rss_kb = rss_mb * 1024
			}
			label == 'cgen' || label.starts_with('cgen (') {
				result.cgen_ms = ms
				result.cgen_rss_kb = rss_mb * 1024
			}
			else {}
		}
	}
	if result.parse_ms == 0 && result.check_ms == 0 && result.cgen_ms == 0 {
		return error('could not parse v3 stage timings from output:\n${output}')
	}
	return result
}

fn rss_mb_from_v3_line(line string) int {
	rss_end := line.index(' MB RSS') or { return 0 }
	return line[..rss_end].trim_space().all_after_last(' ').int()
}

// parse_vlines returns the number of parsed V lines that `output` reports, or 0.
pub fn parse_vlines(output string) int {
	for raw_line in output.split_into_lines() {
		line := raw_line.trim_space()
		if line.starts_with('parsed .v lines') {
			return line.trim_string_left('parsed .v lines').trim_space().all_before(' ').int()
		}
		if line.contains('V') && line.contains('source') && line.contains('size:') {
			start := line.index(':') or { continue }
			end := line.index('lines,') or { continue }
			return line[start + 1..end].trim_space().int()
		}
	}
	return 0
}
