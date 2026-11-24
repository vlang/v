module driver

// A build that restarts itself after its checker printed warnings, for example to
// leave the module cache, must tell the new process, or the warnings appear twice.
fn test_restart_forwards_printed_checker_notices_once() {
	assert v3_internal_checker_notices_printed_flag !in v3_restart_diagnostics_flags([])
	v3_checker_notices_printed(true)
	assert v3_restart_diagnostics_flags([]) == [v3_internal_checker_notices_printed_flag]
	assert v3_restart_diagnostics_flags(['-nocache', v3_internal_checker_notices_printed_flag]) == []string{}
	v3_parser_diagnostics_printed(true)
	assert v3_restart_diagnostics_flags(['file.v']) == [
		v3_internal_parser_diagnostics_printed_flag,
		v3_internal_checker_notices_printed_flag,
	]
}
