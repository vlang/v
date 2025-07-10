module log

import term

// Level defines the possible log levels, used by Log.set_level()
pub enum Level {
	disabled = 0   // lowest level, disables everything else
	fatal // disables error, warn, info and debug
	error // disables warn, info and debug
	warn  // disables info and debug
	info  // disables debug
	debug
}

// LogTarget defines the possible log targets, that Log supports
pub enum LogTarget {
	console
	file
	both
}

// tag_to_console returns the tag for log level `l` as a colored string.
@[noinline]
fn tag_to_console(l Level, short_tag bool) string {
	if short_tag {
		return match l {
			.disabled { ' ' }
			.fatal { term.red('F') }
			.error { term.red('E') }
			.warn { term.yellow('W') }
			.info { term.white('I') }
			.debug { term.magenta('D') }
		}
	} else {
		return match l {
			.disabled { '' }
			.fatal { term.red('FATAL') }
			.error { term.red('ERROR') }
			.warn { term.yellow('WARN ') }
			.info { term.white('INFO ') }
			.debug { term.magenta('DEBUG') }
		}
	}
}

// tag_to_file returns the tag for log level `l` as a string.
@[noinline]
fn tag_to_file(l Level, short_tag bool) string {
	if short_tag {
		return match l {
			.disabled { ' ' }
			.fatal { 'F' }
			.error { 'E' }
			.warn { 'W' }
			.info { 'I' }
			.debug { 'D' }
		}
	} else {
		return match l {
			.disabled { '     ' }
			.fatal { 'FATAL' }
			.error { 'ERROR' }
			.warn { 'WARN ' }
			.info { 'INFO ' }
			.debug { 'DEBUG' }
		}
	}
}

// level_from_tag returns the log level from the given string.
// It returns `none` when it does not find a match.
@[noinline]
pub fn level_from_tag(tag string) ?Level {
	return match tag {
		'DISABLED', ' ' { Level.disabled }
		'FATAL', 'F' { Level.fatal }
		'ERROR', 'E' { Level.error }
		'WARN', 'W' { Level.warn }
		'INFO', 'I' { Level.info }
		'DEBUG', 'D' { Level.debug }
		else { none }
	}
}

// target_from_label returns the log target from the given string.
// It returns `none` when it does not find a match.
@[noinline]
pub fn target_from_label(label string) ?LogTarget {
	return match label {
		'console' { LogTarget.console }
		'file' { LogTarget.file }
		'both' { LogTarget.both }
		else { none }
	}
}
