module util

fn test_tool_recompilation_args_force_system_cc_for_vdoc_on_freebsd() {
	assert tool_recompilation_args('vdoc', 'freebsd') == ['-cc', 'cc']
}

fn test_tool_recompilation_args_do_not_change_other_tools_or_platforms() {
	assert tool_recompilation_args('vfmt', 'freebsd').len == 0
	assert tool_recompilation_args('vdoc', 'linux').len == 0
}
