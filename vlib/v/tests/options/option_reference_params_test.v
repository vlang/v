struct Logger {}

fn (l Logger) log(this string) {
	println(this)
}

fn new_logger() &Logger {
	return &Logger{}
}

fn do_log(mut maybe_logr ?&Logger, this string) {
	if logr := maybe_logr {
		logr.log(this)
	}
}

fn bang(mut logr ?&Logger, this string) {
	nested_log := fn [mut logr] (this string) {
		do_log(mut logr, this)
	}
	nested_log(this)
}

fn test_option_reference_params() {
	mut logr := new_logger()
	bang(mut &logr, 'bang!')
	assert true
}
