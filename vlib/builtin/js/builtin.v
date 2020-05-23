// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

fn JS.console.log(arg ...string)
fn JS.process.stdout.write(arg string)

pub fn println(s any) {
	JS.console.log(s)
}

pub fn print(s any) {
	JS.process.stdout.write(s)
}