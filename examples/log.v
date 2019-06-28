import log

fn main(){
    mut l := log.Log{level:log.INFO}
    l.info('info')
    l.warn('warn')
    l.error('error')
    l.debug('no debug')
    l.set_level(log.DEBUG)
    l.debug('debug')
    l.fatal('fatal')
}
