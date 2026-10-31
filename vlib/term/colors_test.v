module term

import strings

const esc = '\x1b['

fn test_format_esc_wraps_a_single_code() {
	assert format_esc('0') == '${esc}0m'
	assert format_esc('31') == '${esc}31m'
	assert format_esc('1;31') == '${esc}1;31m'
}

fn test_format_wraps_msg_between_open_and_close() {
	assert format('hi', '9', '29') == '${esc}9mhi${esc}29m'
	assert format('hi', '', '') == '${esc}mhi${esc}m'
}

fn test_format_rgb_embeds_the_24_bit_color() {
	assert format_rgb(0, 255, 0, 'hi', '38', '39') == '${esc}38;2;0;255;0mhi${esc}39m'
	assert format_rgb(255, 128, 0, 'x', '48', '49') == '${esc}48;2;255;128;0mx${esc}49m'
}

fn test_rgb_sets_the_foreground() {
	assert rgb(0, 255, 0, 'hi') == '${esc}38;2;0;255;0mhi${esc}39m'
	assert rgb(255, 0, 0, 'x') == '${esc}38;2;255;0;0mx${esc}39m'
}

fn test_bg_rgb_sets_the_background() {
	assert bg_rgb(255, 0, 0, 'hi') == '${esc}48;2;255;0;0mhi${esc}49m'
	assert bg_rgb(0, 0, 255, 'x') == '${esc}48;2;0;0;255mx${esc}49m'
}

fn test_hex_splits_the_color_into_rgb_components() {
	assert hex(0x00FF00, 'hi') == '${esc}38;2;0;255;0mhi${esc}39m'
	assert hex(0xFF8000, 'x') == '${esc}38;2;255;128;0mx${esc}39m'
	assert hex(0x0000FF, 'hi') == '${esc}38;2;0;0;255mhi${esc}39m'
}

fn test_bg_hex_splits_the_color_into_rgb_components() {
	assert bg_hex(0x0000FF, 'hi') == '${esc}48;2;0;0;255mhi${esc}49m'
	assert bg_hex(0x00FF00, 'x') == '${esc}48;2;0;255;0mx${esc}49m'
}

fn test_reset_clears_all_attributes() {
	assert reset('hi') == '${esc}0mhi${esc}0m'
}

fn test_bold_and_dim() {
	assert bold('hi') == '${esc}1mhi${esc}22m'
	assert dim('hi') == '${esc}2mhi${esc}22m'
}

fn test_italic_and_underline() {
	assert italic('hi') == '${esc}3mhi${esc}23m'
	assert underline('hi') == '${esc}4mhi${esc}24m'
}

fn test_blink_styles() {
	assert slow_blink('hi') == '${esc}5mhi${esc}25m'
	assert rapid_blink('hi') == '${esc}6mhi${esc}26m'
}

fn test_inverse_and_hidden() {
	assert inverse('hi') == '${esc}7mhi${esc}27m'
	assert hidden('hi') == '${esc}8mhi${esc}28m'
}

fn test_strikethrough() {
	assert strikethrough('hi') == '${esc}9mhi${esc}29m'
}

fn test_foreground_colors() {
	assert black('hi') == '${esc}30mhi${esc}39m'
	assert red('hi') == '${esc}31mhi${esc}39m'
	assert green('hi') == '${esc}32mhi${esc}39m'
	assert yellow('hi') == '${esc}33mhi${esc}39m'
	assert blue('hi') == '${esc}34mhi${esc}39m'
	assert magenta('hi') == '${esc}35mhi${esc}39m'
	assert cyan('hi') == '${esc}36mhi${esc}39m'
	assert white('hi') == '${esc}37mhi${esc}39m'
}

fn test_background_colors() {
	assert bg_black('hi') == '${esc}40mhi${esc}49m'
	assert bg_red('hi') == '${esc}41mhi${esc}49m'
	assert bg_green('hi') == '${esc}42mhi${esc}49m'
	assert bg_yellow('hi') == '${esc}43mhi${esc}49m'
	assert bg_blue('hi') == '${esc}44mhi${esc}49m'
	assert bg_magenta('hi') == '${esc}45mhi${esc}49m'
	assert bg_cyan('hi') == '${esc}46mhi${esc}49m'
	assert bg_white('hi') == '${esc}47mhi${esc}49m'
}

fn test_bright_foreground_colors_close_on_the_default_foreground() {
	assert bright_black('hi') == '${esc}90mhi${esc}39m'
	assert bright_red('hi') == '${esc}91mhi${esc}39m'
	assert bright_green('hi') == '${esc}92mhi${esc}39m'
	assert bright_yellow('hi') == '${esc}93mhi${esc}39m'
	assert bright_blue('hi') == '${esc}94mhi${esc}39m'
	assert bright_magenta('hi') == '${esc}95mhi${esc}39m'
	assert bright_cyan('hi') == '${esc}96mhi${esc}39m'
	assert bright_white('hi') == '${esc}97mhi${esc}39m'
}

fn test_bright_background_colors() {
	assert bright_bg_black('hi') == '${esc}100mhi${esc}49m'
	assert bright_bg_red('hi') == '${esc}101mhi${esc}49m'
	assert bright_bg_green('hi') == '${esc}102mhi${esc}49m'
	assert bright_bg_yellow('hi') == '${esc}103mhi${esc}49m'
	assert bright_bg_blue('hi') == '${esc}104mhi${esc}49m'
	assert bright_bg_magenta('hi') == '${esc}105mhi${esc}49m'
	assert bright_bg_cyan('hi') == '${esc}106mhi${esc}49m'
	assert bright_bg_white('hi') == '${esc}107mhi${esc}49m'
}

fn test_gray_is_an_alias_for_bright_black() {
	assert gray('hi') == bright_black('hi')
	assert gray('hi') == '${esc}90mhi${esc}39m'
}

fn test_highlight_command_wraps_the_command_in_spaces_and_two_colors() {
	assert highlight_command('v run') == '${esc}97m${esc}46m v run ${esc}49m${esc}39m'
}

fn test_write_color_without_any_config_passes_the_text_through() {
	mut sb := strings.new_builder(16)
	write_color(mut sb, 'hi', ColorConfig{})
	assert sb.str() == 'hi'
}

fn test_write_color_joins_style_fg_and_bg_codes() {
	mut sb := strings.new_builder(64)
	write_color(mut sb, 'hi', styles: [.bold, .italic], fg: .red, bg: .cyan)
	assert sb.str() == '${esc}1;3;31;46mhi${esc}0m'
}

fn test_write_color_honours_a_custom_code() {
	mut sb := strings.new_builder(64)
	write_color(mut sb, 'hi', custom: '38;5;214')
	assert sb.str() == '${esc}38;5;214mhi${esc}0m'
}

fn test_write_color_uses_the_numeric_value_of_each_style() {
	mut sb := strings.new_builder(64)
	write_color(mut sb, 'hi', styles: [.dim, .underline, .blink, .reverse])
	assert sb.str() == '${esc}2;4;5;7mhi${esc}0m'
}

fn test_writeln_color_appends_a_newline() {
	mut sb := strings.new_builder(64)
	writeln_color(mut sb, 'hi', fg: .red)
	assert sb.str() == '${esc}31mhi${esc}0m\n'
}

fn test_writeln_color_without_config_still_terminates_the_line() {
	mut sb := strings.new_builder(16)
	writeln_color(mut sb, 'hi', ColorConfig{})
	assert sb.str() == 'hi\n'
}
