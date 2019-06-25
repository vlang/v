import log

fn main(){
    mut l := log.Log{level:log.INFO}
    l.i('info')
    l.w('warn')
    l.e('error')
    l.d('no debug')
    l.set_level(log.DEBUG)
    l.d('debug')
    l.f('fatal')
}