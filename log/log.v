module log

const (
    FATAL = 1
    ERROR = 2 
    WARN = 3
    INFO = 4
    DEBUG =5
)

struct Log{
mut:
    level int
}


pub fn (l mut Log) set_level(level int){
    l.level = level
}

pub fn (l Log) f(s string){
    panic(s)
}

pub fn (l Log) e(s string){
    if l.level >= ERROR{
        f := red('E')
        println('[$f]$s')
    }
}

pub fn (l Log) w(s string){
    if l.level >= WARN{
        f := yellow('W')
        println('[$f]$s')
    }
}

pub fn (l Log) i(s string){
    if l.level >= INFO{
        f := white('I')
        println('[$f]$s')
    }
}

pub fn (l Log) d(s string){
    if l.level >= DEBUG{
        f := blue('D')
        println('[$f]$s')
    }
}