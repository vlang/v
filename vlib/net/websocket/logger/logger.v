module logger

const (
	colors = {
		"success": "\e[32",
		"debug": "\e[36",
		"error": "\e[91",
		"warn": "\e[33",
		"critical": "\e[31",
		"fatal": "\e[31",
		"info": "\e[37"
	}
)

struct Logger {
	mod string
}

pub fn new(mod string) &Logger {
	return &Logger{mod: mod}
}

pub fn (l &Logger) d(message string){
	$if debug {
		l.print("debug", message)
	}
}

pub fn (l &Logger) i(message string){
	l.print('info', message)
}

pub fn (l &Logger) e(message string){
	l.print('error', message)
}

pub fn (l &Logger) c(message string){
	l.print('critical', message)
}

pub fn (l &Logger) f(message string){
	l.print('fatal', message)
	exit(-1)
}

pub fn (l &Logger) w(message string){
	l.print('warn', message)
}

pub fn (l &Logger) s(message string) {
	l.print('success', message)
}

fn (l &Logger) print(mod, message string) {
	println('${colors[mod]};7m[${mod}]\e[0m \e[1m${l.mod}\e[0m: ${message}')
}