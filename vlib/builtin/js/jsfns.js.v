// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This file contains JS functions present in both node and the browser.
// They have been ported from their TypeScript definitions.

module builtin

pub interface JS.Object {}

[single_impl]
pub interface JS.BigInt {
	JS.Any
}

[single_impl]
pub interface JS.Number {
	JS.Any
}

[single_impl]
pub interface JS.String {
	JS.Any
	length JS.Number
	charAt(index JS.Number) JS.String
	charCodeAt(index JS.Number) JS.Number
	toUpperCase() JS.String
	toLowerCase() JS.String
	concat(a JS.String) JS.String
	includes(substr JS.String) JS.Boolean
	endsWith(substr JS.String) JS.Boolean
	startsWith(substr JS.String) JS.Boolean
	slice(a JS.Number, b JS.Number) JS.String
	split(dot JS.String) JS.Array
	indexOf(needle JS.String) JS.Number
	lastIndexOf(needle JS.String) JS.Number
}

[single_impl]
pub interface JS.Boolean {
	JS.Any
	length JS.Number
}

pub interface JS.Map {
	JS.Any
	size JS.Number
	clear()
	delete(key JS.Any) JS.Boolean
	get(key JS.Any) JS.Any
	has(key JS.Any) JS.Any
	set(key JS.Any, val JS.Any)
}

#function Any(val) { return val; }

pub interface JS.Any {}

pub fn js_is_null(x JS.Any) bool {
	res := false
	#res.val = x === null

	return res
}

pub fn js_is_undefined(x JS.Any) bool {
	res := false
	#res.val = x === undefined

	return res
}

pub fn js_null() JS.Any {
	mut obj := JS.Any{}
	#obj = null;

	return obj
}

pub fn js_undefined() JS.Any {
	mut obj := JS.Any{}
	#obj = undefined;

	return obj
}

pub interface JS.Array {
	JS.Any // map(fn (JS.Any) JS.Any) JS.Array
	map(JS.Any) JS.Array
	push(JS.Any) JS.Any
	pop() JS.Any
	at(JS.Number) JS.Any
mut:
	length JS.Number
}

pub fn JS.Array.prototype.constructor(...any) JS.Array

// browser: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Error
// node: https://nodejs.org/api/errors.html#errors_class_error
pub struct JS.Error {
pub:
	name    string
	message string
	stack   string
}

// Type prototype functions
fn (v JS.String) toString() JS.String
fn (v JS.Number) toString() JS.String
fn (v JS.Boolean) toString() JS.String
fn (v JS.Array) toString() JS.String
fn (v JS.Map) toString() JS.String

// Hack for "`[]JS.String` is not a struct" when returning arr.length or arr.len
// TODO: Fix []JS.String not a struct error
fn native_str_arr_len(arr []JS.String) int {
	len := 0
	#len = arr.length

	return len
}

// Top level functions
fn JS.eval(JS.String) JS.Any
fn JS.parseInt(JS.String, f64) JS.Number
fn JS.parseFloat(JS.String) JS.Number
fn JS.isNaN(f64) bool
fn JS.isFinite(f64) bool
fn JS.decodeURI(JS.String) JS.String
fn JS.decodeURIComponent(JS.String) JS.String
fn JS.encodeURI(JS.String) JS.String

type EncodeURIComponentArg = JS.String | bool | f64

fn JS.encodeURIComponent(EncodeURIComponentArg) JS.String
fn JS.escape(JS.String) JS.String
fn JS.unescape(JS.String) JS.String

// console
fn JS.console.assert(bool, ...any)
fn JS.console.clear()
fn JS.console.count(JS.String)
fn JS.console.countReset(JS.String)
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
fn JS.console.time(JS.String)
fn JS.console.timeEnd(JS.String)
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
fn JS.JSON.stringify(any) JS.String
fn JS.JSON.parse(string) any
