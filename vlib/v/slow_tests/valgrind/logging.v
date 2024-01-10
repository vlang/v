import log
import time

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
	for i := 0; i < 100; i++ {
		l.info('123456')
		time.sleep(1 * time.millisecond)
	}
}
