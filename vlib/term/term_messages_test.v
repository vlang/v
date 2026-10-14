module term

fn test_can_show_color_is_stable_across_repeated_calls() {
	assert can_show_color_on_stdout() == can_show_color_on_stdout()
	assert can_show_color_on_stderr() == can_show_color_on_stderr()
}

fn test_colorize_only_wraps_the_message_when_stdout_supports_colour() {
	supported := can_show_color_on_stdout()
	assert colorize(red, 'hi') == if supported { red('hi') } else { 'hi' }
	assert colorize(bold, 'hi') == if supported { bold('hi') } else { 'hi' }
}

fn test_ecolorize_only_wraps_the_message_when_stderr_supports_colour() {
	supported := can_show_color_on_stderr()
	assert ecolorize(red, 'hi') == if supported { red('hi') } else { 'hi' }
	assert ecolorize(bright_red, 'hi') == if supported { bright_red('hi') } else { 'hi' }
}

fn test_the_message_helpers_follow_the_stdout_capability() {
	supported := can_show_color_on_stdout()
	assert ok_message('hi') == if supported { green('hi') } else { 'hi' }
	assert warn_message('hi') == if supported { bright_yellow('hi') } else { 'hi' }
	assert failed('hi') == if supported { bg_red(bold(white('hi'))) } else { 'hi' }
}

fn test_fail_message_is_failed() {
	assert fail_message('boom') == failed('boom')
}

fn test_message_helpers_return_the_message_unchanged_when_colour_is_off() {
	if can_show_color_on_stdout() {
		return
	}
	assert failed('boom') == 'boom'
	assert ok_message('good') == 'good'
	assert warn_message('careful') == 'careful'
	assert fail_message('bad') == 'bad'
	assert colorize(red, 'msg') == 'msg'
}
