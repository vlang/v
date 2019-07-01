module log

import term

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

pub fn (l Log) fatal(s string){
    panic(s)
}

pub fn (l Log) error(s string){
    if l.level >= ERROR{
        f := term.red('E')
        println('[$f]$s')
    }
}

pub fn (l Log) warn(s string){
    if l.level >= WARN{
        f := term.yellow('W')
        println('[$f]$s')
    }
}

pub fn (l Log) info(s string){
    if l.level >= INFO{
        f := term.white('I')
        println('[$f]$s')
    }
}

pub fn (l Log) debug(s string){
    if l.level >= DEBUG{
        f := term.blue('D')
        println('[$f]$s')
    }
}