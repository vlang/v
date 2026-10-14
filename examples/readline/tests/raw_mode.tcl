# Wait for the program to enable raw mode before sending input to its terminal.
proc wait_for_raw_mode {timeout_seconds} {
    global spawn_out
    if {[info exists spawn_out(replica,name)]} {
        set tty $spawn_out(replica,name)
    } else {
        set tty $spawn_out(slave,name)
    }
    set deadline [expr {[clock milliseconds] + 1000 * $timeout_seconds}]
    while {[clock milliseconds] < $deadline} {
        if {[catch {exec stty -a < $tty} mode]} {
            return 0
        }
        if {[string match "*-icanon*" $mode]} {
            return 1
        }
        after 25
    }
    return 0
}
