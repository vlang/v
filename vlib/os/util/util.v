module util

import os

// TODO `select` doesn't work with time.Duration for some reason
pub fn execute_with_timeout(cmd string, timeout i64) ?os.Result {
	ch := chan os.Result{cap: 1}
	spawn fn [cmd] (c chan os.Result) {
		res := os.execute(cmd)
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
