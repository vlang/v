// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This file contains JS functions present in both node and the browser.
// They have been ported from their TypeScript definitions.

module builtin

pub struct JS.Number {}
pub struct JS.String {
	length JS.Number
}
pub struct JS.Boolean {}
pub struct JS.Array {}
pub struct JS.Map {}

// Type prototype functions
fn (v JS.String) toString() JS.String
fn (v JS.Number) toString() JS.String
fn (v JS.Boolean) toString() JS.String
fn (v JS.Array) toString() JS.String
fn (v JS.Map) toString() JS.String

fn (v JS.String) slice(a int, b int) JS.String

// Top level functions
fn JS.eval(string) any
fn JS.parseInt(string, f64) f64
fn JS.parseFloat(string) f64
fn JS.isNaN(f64) bool
fn JS.isFinite(f64) bool
fn JS.decodeURI(string) string
fn JS.decodeURIComponent(string) string
fn JS.encodeURI(string) string
type EncodeURIComponentArg = string | f64 | bool
fn JS.encodeURIComponent(EncodeURIComponentArg) string
fn JS.escape(string) string
fn JS.unescape(string) string

// console
fn JS.console.assert(bool, ...any)
fn JS.console.clear()
fn JS.console.count(string)
fn JS.console.countReset(string)
fn JS.console.debug(...any)
fn JS.console.dir(any, any)
fn JS.console.dirxml(...any)
fn JS.console.error(...any)
fn JS.console.exception(string, ...any)
fn JS.console.group(...any)
fn JS.console.groupCollapsed(...any)
fn JS.console.groupEnd()
fn JS.console.info(...any)
fn JS.console.log(...any)
fn JS.console.table(any, []string)
fn JS.console.time(string)
fn JS.console.timeEnd(string)
fn JS.console.timeLog(string, ...any)
fn JS.console.timeStamp(string)
fn JS.console.trace(...any)
fn JS.console.warn(...any)

// Math
fn JS.Math.abs(f64) f64
fn JS.Math.acos(f64) f64
fn JS.Math.asin(f64) f64
fn JS.Math.atan(f64) f64
fn JS.Math.atan2(f64, f64) f64
fn JS.Math.ceil(f64) f64
fn JS.Math.cos(f64) f64
fn JS.Math.exp(f64) f64
fn JS.Math.floor(f64) f64
fn JS.Math.log(f64) f64
fn JS.Math.max(...f64) f64
fn JS.Math.min(...f64) f64
fn JS.Math.pow(f64, f64) f64
fn JS.Math.random() f64
fn JS.Math.round(f64) f64
fn JS.Math.sin(f64) f64
fn JS.Math.sqrt(f64) f64
fn JS.Math.tan(f64) f64

// JSON
fn JS.JSON.stringify(any) string
fn JS.JSON.parse(string) any
