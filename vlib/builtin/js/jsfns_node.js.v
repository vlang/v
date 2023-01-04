// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This file contains JS functions only present in node.js.
// They have been ported from their TypeScript definitions.

module builtin

pub struct JS.node_process {
pub:
	arch     string
	argsv    []string
	env      []string
	platform string
	version  string
	// TODO: add all properties
}

// hack to access  process properties
pub fn js_node_process() JS.node_process {
	#return process

	return JS.node_process{}
}

fn JS.process.exit(int)
fn JS.process.stdout.write(string) bool
fn JS.process.stdout.writeln(string) bool
fn JS.process.stderr.write(string) bool
fn JS.process.stderr.writeln(string) bool
