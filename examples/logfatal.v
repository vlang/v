import log

[noreturn]
fn should_not_return(mut logger log.Log) {
	logger.fatal('${@FILE_LINE}: yikes!')
}

fn main() {
	mut my_log := log.Log{}
	should_not_return(mut my_log)
}
