import log
import time

fn test_use_stdout_respects_default_log_level() {
	slevel := $d('log_default_level', 'info')
	expected := log.level_from_tag(slevel.to_upper()) or { panic('invalid test log level') }
	log.set_level(.disabled)
	log.use_stdout()
	assert log.get_level() == expected
}

fn test_default_log_instance() {
	println(@FN + ' start')
	log.info('info')
	log.warn('warn')
	log.error('error')
	log.debug('no output for debug')
	println('^^^ there should be no `no output for debug` shown above')
	log.set_level(.debug)
	log.debug('debug now')
	println('^^^ there should be `debug now` shown above')
	log.set_level(log.level_from_tag('INFO') or { log.Level.disabled })
	log.info('info again')
	log.debug('no output for debug')
	log.set_level(log.level_from_tag('') or { log.Level.disabled })
	log.error('no output anymore')
	println('^^^ there should be no `no output anymore` shown above')
	println(@FN + ' end')
}

fn log_messages(tidx int) {
	time.sleep(2 * time.millisecond)
	for i in 0 .. 3 {
		log.debug('hi from thread ${tidx}')
	}
}

fn test_default_log_instance_used_in_multiple_threads() {
	eprintln('\n${@METHOD} start')
	log.set_level(.debug)
	mut threads := []thread{}
	for tidx in 0 .. 3 {
		threads << spawn log_messages(tidx)
	}
	threads.wait()
}
