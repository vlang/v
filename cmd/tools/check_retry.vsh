#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp

// This script is used by cmd/tools/vretry_test.v, to check that the `v retry`
// subcommand works as expected, without relying on external commands like
// `git`, or on non portable ones like `true`/`false` or `echo`.

fn main() {
	args := arguments()#[1..]
	println(args)
	if args == ['too', 'many', 'arguments'] {
		exit(1)
	}
}
