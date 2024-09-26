## This file will be included by all other .expect scripts in this folder.
set timeout 2
set stty_init {rows 24 cols 80}
expect_after timeout { exit 1}
set test_file [lindex $argv 1]
spawn [lindex $argv 0]
