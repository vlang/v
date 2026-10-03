module util

import os

// TODO `select` doesn't work with time.Duration for some reason
@[deprecated: 'use os.util.exec_with_timeout with an argument array; command strings can allow shell injection']
pub fn execute_with_timeout(cmd string, timeout i64) ?os.Result {
	ch := chan os.Result{cap: 1}
	spawn fn [cmd] (c chan os.Result) {
		res := os.exec(if os.user_os() == 'windows' {
			['cmd.exe', '/d', '/s', '/c', cmd]
		} else {
			['sh', '-c', cmd]
		})
		c <- res
	}(ch)
	select {
		a := <-ch {
			return a
		}
		// timeout {
		// 1000 * time.millisecond {
		// timeout * time.millisecond {
		timeout * 1_000_000 {
			return none
		}
	}
	return os.Result{}
}

// exec_with_timeout runs literal arguments, returning none when the timeout elapses.
// The timeout is in milliseconds and does not terminate the child process.
pub fn exec_with_timeout(args []string, timeout i64) ?os.Result {
	ch := chan os.Result{cap: 1}
	spawn fn [args] (c chan os.Result) {
		res := os.exec(args)
		c <- res
	}(ch)
	select {
		a := <-ch {
			return a
		}
		// timeout {
		// 1000 * time.millisecond {
		// timeout * time.millisecond {
		timeout * 1_000_000 {
			return none
		}
	}
	return os.Result{}
}
