import log

fn main() {
	mut l := log.Log{}
	l.set_level(log.INFO)
	// Make a new file called info.log in the current folder
	l.set_output_label('info')
	l.set_output_path('.')
  
	l.info('info')
	l.warn('warn')
	l.error('error')
	l.debug('no debug')
	l.set_level(log.DEBUG)
	l.debug('debug')
	l.fatal('fatal')
}
