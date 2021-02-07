module main

import os

fn exec(args []string) {
	os.execve('/bin/bash', args, []) or {
		// eprintln(err)
		panic(err)
	}
}

fn main() {
	// exec(["-c","find /"]) //works
	exec(['-c', 'find /tmp/']) // here it works as well

	// exec(["-c","find","/tmp/"])  // does not work I guess is normal
}
