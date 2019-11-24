import log

fn main() {
	mut l := Log { LogLevel.info, 'info', true }
	l.info('info')
	l.warn('warn')
	l.error('error')
	l.debug('no debug')
	l.set_level(DEBUG)
	l.debug('debug')
	l.fatal('fatal')
}
