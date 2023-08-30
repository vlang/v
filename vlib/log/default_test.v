import log

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
	log.set_level(log.level_from_tag('') or { log.Level.disabled })
	log.error('no output anymore')
	println('^^^ there should be no `no output anymore` shown above')
	println(@FN + ' end')
}
