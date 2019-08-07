module log

import os
import time
import term

const (
    FATAL = 1
    ERROR = 2 
    WARN  = 3
    INFO  = 4
    DEBUG = 5
)

interface Logger {
    fatal(s string)
    error(s string)
    warn(s string)
    info(s string)
    debug(s string)
}

struct Log{
mut:
    level int
    output string
}


pub fn (l mut Log) set_level(level int){
    l.level = level
}

pub fn (l mut Log) set_output(output string) {
    l.output = output
}

fn (l Log) log_file(s string, e string) {
    filename := l.output
    f := os.open_append(l.output) or {
        panic('error reading file $filename')
        return
    }
    timestamp := time.now().format_ss()
    f.writeln('$timestamp [$e] $s')
}

pub fn (l Log) fatal(s string){
    panic(s)
}

pub fn (l Log) error(s string){
    if l.level >= ERROR{
        switch l.output {
        case 'terminal':
            f := term.red('E')
            t := time.now()
            println('[$f ${t.format()}] $s')

        default:
            l.log_file(s, 'E')
        }
    }
}

pub fn (l Log) warn(s string){
    if l.level >= WARN{
        switch l.output {
        case 'terminal':
            f := term.yellow('W')
            t := time.now()
            println('[$f ${t.format()}] $s')

        default:
            l.log_file(s, 'W')
        }
    }  
}

pub fn (l Log) info(s string){
    if l.level >= INFO{
        switch l.output {
        case 'terminal':
            f := term.white('I')
            t := time.now()
            println('[$f ${t.format()}] $s')

        default:
            l.log_file(s, 'I')
        }
    }
}

pub fn (l Log) debug(s string){
    if l.level >= DEBUG{
        switch l.output {
        case 'terminal':
            f := term.blue('D')
            t := time.now()
            println('[$f ${t.format()}] $s')

        default:
            l.log_file(s, 'D')
        }
    }
}
