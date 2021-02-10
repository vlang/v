// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This file contains JS functions only present in node.js.
// They have been ported from their TypeScript definitions.

module builtin

fn JS.process.exit(int)
fn JS.process.stdout.write(string) bool
fn JS.process.stdout.writeln(string) bool
fn JS.process.stderr.write(string) bool
fn JS.process.stderr.writeln(string) bool
