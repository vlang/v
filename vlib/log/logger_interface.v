module log

// Logger is an interface that describes a generic Logger
pub interface Logger {
	get_level() Level
mut:
	fatal(s string)
	error(s string)
	warn(s string)
	info(s string)
	debug(s string)
	// utility methods:
	set_level(level Level)
	free()
}
