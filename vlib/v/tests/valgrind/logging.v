import log
import os

fn main() {
	mut l := log.Log{}
	defer {
		l.close()
	}
	l.set_level(.info)
	l.info('info')
	l.warn('warn')
	l.error('an error')
	l.debug('some debug info')
}
