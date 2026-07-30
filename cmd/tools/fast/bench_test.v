module main

fn test_parse_v1_stage_measurements() {
	output := '24  ms SCAN
106  ms PARSE
123  ms CHECK
75  ms C GEN
V  source  code size: 196376 lines, 100 tokens, 1000 bytes'
	stages := parse_stage_measurements(output) or { panic(err) }
	assert stages.scan_ms == 24
	assert stages.parse_ms == 106
	assert stages.check_ms == 123
	assert stages.cgen_ms == 75
	assert stages.vlines == 196376
	assert stages.scan_rss_kb == 0
	assert stages.cgen_rss_kb == 0
}

fn test_parse_v3_stage_measurements() {
	output := '=== v3 benchmark ===
  parse setup/cache        6.18 ms       26 MB RSS       24 MB physical footprint       26 MB peak
  parse .vh                0.00 ms       26 MB RSS       24 MB physical footprint       26 MB peak
  parse .v (parallel)     23.04 ms       31 MB RSS       29 MB physical footprint       31 MB peak
  resolve imports          0.74 ms       32 MB RSS       30 MB physical footprint       32 MB peak
    parsed .v lines              10616 lines
  check                   32.65 ms       37 MB RSS       33 MB physical footprint       37 MB peak
  cgen                    32.48 ms       49 MB RSS       40 MB physical footprint       49 MB peak'
	stages := parse_stage_measurements(output) or { panic(err) }
	assert stages.scan_ms == 6
	assert stages.parse_ms == 23
	assert stages.check_ms == 32
	assert stages.cgen_ms == 32
	assert stages.vlines == 10616
	assert stages.scan_rss_kb == 26 * 1024
	assert stages.parse_rss_kb == 32 * 1024
	assert stages.check_rss_kb == 37 * 1024
	assert stages.cgen_rss_kb == 49 * 1024
}
