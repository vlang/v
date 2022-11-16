module builtin

import strings
/// Internal representation of `array` type. It is used to implement slices and to make slices behave correctly
/// it simply stores reference to original array and to index them properly it does index array relative to `index_start`.

struct array_buffer {
	arr         JS.Array
	index_start int
	len         int
	cap         int
	has_slice   bool
}

fn (mut a array_buffer) make_copy() {
	if a.index_start != 0 || a.has_slice {
		mut new_arr := JS.makeEmtpyJSArray()
		for i in 0 .. a.len {
			#new_arr.push(a.val.get(i))

			mut x := i
			x = x
		}
		new_arr = new_arr
		#a.val.arr = new_arr
		#a.val.index_start = new int(0)
		#a.val.has_slice = new bool(false)
	}
}

#array_buffer.prototype.make_copy = function() { return array_buffer_make_copy(this) }
// TODO(playX): Should this be implemented fully in JS, use generics or just voidptr?
fn (a array_buffer) get(ix int) voidptr {
	mut res := unsafe { nil }
	#res = a.arr[a.index_start.val + ix.val];

	return res
}

fn (mut a array_buffer) set(ix int, val voidptr) {
	#a.val.arr[a.val.index_start.valueOf() + ix.valueOf()] = val;
}

#array_buffer.prototype.get = function(ix) { return array_buffer_get(this,ix);}
#array_buffer.prototype.set = function(ix,val) { array_buffer_set(this,ix,val); }

struct array {
pub mut:
	arr array_buffer

	len int
	cap int
}

fn v_sort(mut arr array, comparator fn (voidptr, voidptr) int) {
	mut need_iter := true
	for need_iter {
		need_iter = false
		for i := 1; i < arr.len; i++ {
			if comparator(arr[i], arr[i - 1]) != 1 {
				tmp := arr[i]
				arr[i] = arr[i - 1]
				arr[i - 1] = tmp
				need_iter = true
			}
		}
	}
}

// trim trims the array length to "index" without modifying the allocated data. If "index" is greater
// than len nothing will be changed.
pub fn (mut a array) trim(index int) {
	if index < a.len {
		a.len = index
	}
}

#function flatIntoArray(target, source, sourceLength, targetIndex, depth) {
#"use strict";
#
#for (var sourceIndex = 0; sourceIndex < sourceLength; ++sourceIndex) {
#if (sourceIndex in source) {
#var element = source[sourceIndex];
#if (depth > 0 && Array.isArray(element))
#targetIndex = flatIntoArray(target, element, element.length, targetIndex, depth - 1);
#else {
#target[targetIndex] = element;
#++targetIndex;
#}
#}
#}
#return targetIndex;
#}
#function flatArray(target,depth) {
#var array = target
#var length = array.length;
#var depthNum = 1;
#
#if (depth !== undefined)
#depthNum = +depth
#
#var result = []
#
#flatIntoArray(result, array, length, 0, depthNum);
#return result;
#}

[unsafe]
pub fn (a array) repeat_to_depth(count int, depth int) array {
	if count < 0 {
		panic('array.repeat: count is negative: ${count}')
	}
	mut arr := empty_array()

	if a.len > 0 {
		for _ in 0 .. count {
			for i in 0 .. a.len {
				if depth > 0 {
					// TODO
				} else {
					arr.push(a.arr.get(i))
				}
			}
		}
	}
	return arr
}

// last returns the last element of the array.
pub fn (a array) last() voidptr {
	mut res := unsafe { nil }
	#res = a.arr.get(new int(a.len-1));

	return res
}

fn (a array) get(ix int) voidptr {
	mut result := unsafe { nil }
	#result = a.arr.get(ix)

	return result
}

pub fn (a array) repeat(count int) array {
	unsafe {
		return a.repeat_to_depth(count, 0)
	}
}

#function makeEmptyArray() { return new array(new array_buffer({ arr: [], len: new int(0), index_start: new int(0), cap: new int(0) })); }
#function makeEmtpyJSArray() { return new Array(); }

fn JS.makeEmptyArray() array
fn JS.makeEmtpyJSArray() JS.Array
fn empty_array() array {
	return JS.makeEmptyArray()
}

fn (a &array) set_len(i int) {
	#a.arr.arr.length=i
}

pub fn (mut a array) sort_with_compare(compare voidptr) {
	v_sort(mut a, compare)
}

pub fn (mut a array) sort_with_compare_old(compare voidptr) {
	#a.val.arr.arr.sort(compare)
}

pub fn (mut a array) sort() {
	#a.val.arr.arr.sort($sortComparator)
}

pub fn (a array) index(v string) int {
	for i in 0 .. a.len {
		#if (a.arr.get(i).toString() == v.toString())

		{
			return i
		}
	}
	return -1
}

pub fn (a array) slice(start int, end int) array {
	mut result := a
	#let slice = a.arr.arr.slice(start,end)
	#result = new array(new array_buffer({arr: a.arr.arr, len: new int(slice.length),cap: new int(slice.length),index_start: new int(start),has_slice: new bool(true)}))
	#a.arr.has_slice = true
	//#v_makeSlice(result)

	return result
}

pub fn (mut a array) insert(i int, val voidptr) {
	#a.val.arr.make_copy()
	#a.val.arr.arr.splice(i,0,val)
}

pub fn (mut a array) insert_many(i int, val voidptr, size int) {
	#a.val.arr.arr.splice(i,0,...val.arr.slice(0,+size))
}

fn (mut a array) push(val voidptr) {
	#a.val.arr.make_copy()
	#if (arguments[2] && arguments[2].valueOf()) {a.val.arr.arr.push(...val)} else {
	#a.val.arr.arr.push(val)
	#}
	#a.val.arr.len.val += 1
}

fn v_filter(arr array, callback fn (voidptr) bool) array {
	mut filtered := empty_array()

	for i := 0; i < arr.arr.len; i++ {
		if callback(arr.arr.get(i)) {
			filtered.push(arr.arr.get(i))
		}
	}
	return filtered
}

fn v_map(arr array, callback fn (voidptr) voidptr) array {
	mut maped := empty_array()

	for i := 0; i < arr.arr.len; i++ {
		maped.push(callback(arr.arr.get(i)))
	}

	return maped
}

struct array_iterator {
	ix  int
	end int
	arr JS.Array
}

#array_iterator.prototype.next = function () {
#if (this.ix.val < this.end.val) {
#this.ix.val++;
#return {done: false, value: this.arr.arr.get(new int(this.ix.val-1))}
#} else {
#return {done: true, value: undefined}
#}
#}
#array_iterator.prototype[Symbol.iterator] = function () { return this; }

#array.prototype[Symbol.iterator] = function () { return new array_iterator({ix: new int(0),end: new int(this.arr.len),arr: this}); }
#array.prototype.entries = function () { let result = []; for (let key = this.arr.index_start.val;key < this.arr.len.val;key++) { result.push([new int(key), this.arr.get(new int(key))]); } return result[Symbol.iterator](); }
#array.prototype.map = function(callback) { return v_map(this,callback); }
#array.prototype.filter = function(callback) { return v_filter(this,callback); }
#Object.defineProperty(array.prototype,'cap',{ get: function () { return this.len; } })
#array.prototype.any = function (value) {
#let val ;if (typeof value == 'function') { val = function (x) { return value(x); } } else { val = function (x) { return vEq(x,value); } }
#for (let i = 0;i < this.arr.arr.length;i++)
#if (val(this.arr.get(i)))
#return true;
#
#return false;
#}

#array.prototype.all = function (value) {
#let val ;if (typeof value == 'function') { val = function (x) { return value(x); } } else { val = function (x) { return vEq(x,value); } }
#for (let i = 0;i < this.arr.arr.length;i++)
#if (!val(this.arr.get(i)))
#return false;
#
#return true;
#}
//#Object.defineProperty(array_buffer.prototype,"len", { get: function() {return new int(this.arr.length);}, set: function(l) { this.arr.length = l.valueOf(); } })
//#Object.defineProperty(array_buffer.prototype,"cap", { get: function() {return new int(this.arr.length);}, set: function(l) { this.arr.length = l.valueOf(); } })
#
#
#function v_makeSlice(array) { Object.defineProperty(array,'len', {get: function() { return this.arr.len; }, set: function(l) { this.arr.len = l; }}) }
// delete deletes array element at index `i`.
pub fn (mut a array) delete(i int) {
	a.delete_many(i, 1)
}

fn arr_copy(mut dst array, src array, count int) {
	for i := 0; i < count; i++ {
		dst.arr.set(i, src.arr.get(i))
	}
}

// delete_many deletes `size` elements beginning with index `i`
pub fn (mut a array) delete_many(i int, size int) {
	#a.val.arr.make_copy()
	#a.val.arr.arr.splice(i.valueOf(),size.valueOf())
}

// prepend prepends one value to the array.
pub fn (mut a array) prepend(val voidptr) {
	a.insert(0, val)
}

// prepend_many prepends another array to this array.
[unsafe]
pub fn (mut a array) prepend_many(val voidptr, size int) {
	unsafe { a.insert_many(0, val, size) }
}

pub fn (a array) reverse() array {
	mut res := empty_array()
	#res.arr.arr = Array.from(a.arr).reverse()

	return res
}

pub fn (mut a array) reverse_in_place() {
	#a.val.arr.arr.reverse()
}

#array.prototype.$includes = function (elem) { return this.arr.arr.find(function(e) { return vEq(elem,e); }) !== undefined;}

pub fn (mut a array) clear() {
	#a.val.arr.make_copy()
	#a.val.arr.arr.clear()
}

// reduce executes a given reducer function on each element of the array,
// resulting in a single output value.
pub fn (a array) reduce(iter fn (int, int) int, accum_start int) int {
	mut accum_ := accum_start
	/*#for (let i = 0;i < a.arr.length;i++)  {
	#accum_ = iter(accum_, a.arr[i])
	#}*/
	for i in 0 .. a.len {
		accum_ = iter(accum_, a.get(i))
	}

	return accum_
}

pub fn (mut a array) pop() voidptr {
	mut res := unsafe { nil }
	#a.val.arr.make_copy()
	#res = a.val.arr.arr.pop()
	#a.val.arr.len.val -= 1

	return res
}

pub fn (a array) first() voidptr {
	mut res := unsafe { nil }
	#res = a.arr.get(new int(0))

	return res
}

#array.prototype.toString = function () {
#let res = "["
#for (let i = 0; i < this.arr.arr.length;i++) {
#res += this.arr.get(i).toString();
#if (i != this.arr.arr.length-1)
#res += ', '
#}
#res += ']'
#return res;
#
#}

pub fn (a array) contains(key voidptr) bool

// delete_last effectively removes last element of an array.
pub fn (mut a array) delete_last() {
	#a.val.arr.arr.pop();
}

[unsafe]
pub fn (a &array) free() {
}

// todo: once (a []u8) will work rewrite this
pub fn (a array) bytestr() string {
	res := ''
	#for (let i = 0;i < a.arr.len.valueOf();i++) res.str += String.fromCharCode(a.arr.get(new int(i)))

	return res
}

pub fn (a []string) str() string {
	mut sb := strings.new_builder(a.len * 3)
	sb.write_string('[')
	for i in 0 .. a.len {
		val := a[i]
		sb.write_string("'")
		sb.write_string(val)
		sb.write_string("'")
		if i < a.len - 1 {
			sb.write_string(', ')
		}
	}
	sb.write_string(']')
	res := sb.str()
	return res
}

pub fn (a array) to_js_array() JS.Array {
	tmp := JS.Array.prototype.constructor()
	for i in 0 .. a.len {
		tmp.push(a.arr.get(i))
	}
	return tmp
}

pub fn (a array) to_number_array() JS.Array {
	tmp := JS.Array.prototype.constructor()
	for i in 0 .. a.len {
		elem := a.arr.get(i)
		_ := elem
		#tmp.push(Number(elem.valueOf()));
	}
	return tmp
}

type EveryFn = fn (JS.Number, JS.Number) JS.Boolean

type BigEveryFn = fn (JS.BigInt, JS.Number) JS.Boolean

pub interface JS.TypedArray {
mut:
	byteLength JS.Number
	byteOffset JS.Number
	length JS.Number
}

pub interface JS.Uint8Array {
	JS.TypedArray
	at(index JS.Number) JS.Number
	every(JS.EveryFn) JS.Boolean
}

pub interface JS.Uint16Array {
	JS.TypedArray
	at(index JS.Number) JS.Number
	every(JS.EveryFn) JS.Boolean
}

pub interface JS.Uint32Array {
	JS.TypedArray
	at(index JS.Number) JS.Number
	every(JS.EveryFn) JS.Boolean
}

pub interface JS.BigUint64Array {
	JS.TypedArray
	at(index JS.Number) JS.BigInt
	every(JS.BigEveryFn) JS.Boolean
}

pub interface JS.Int8Array {
	JS.TypedArray
	at(index JS.Number) JS.Number
	every(JS.EveryFn) JS.Boolean
}

pub interface JS.Int16Array {
	JS.TypedArray
	at(index JS.Number) JS.Number
	every(JS.EveryFn) JS.Boolean
}

pub interface JS.Int32Array {
	JS.TypedArray
	at(index JS.Number) JS.Number
	every(JS.EveryFn) JS.Boolean
}

pub interface JS.BigInt64Array {
	JS.TypedArray
	at(index JS.Number) JS.BigInt
	every(JS.BigEveryFn) JS.Boolean
}

pub interface JS.Float32Array {
	JS.TypedArray
	at(index JS.Number) JS.Number
	every(JS.EveryFn) JS.Boolean
}

pub interface JS.Float64Array {
	JS.TypedArray
	at(index JS.Number) JS.Number
	every(JS.EveryFn) JS.Boolean
}

pub fn uint8_array(arr []u8) JS.Uint8Array {
	#let tmp = new Array();

	for elem in arr {
		_ := elem
		#tmp.push(elem.val)
	}
	mut uint_arr := JS.Uint8Array{}
	#uint_arr = new Uint8Array(tmp)

	return uint_arr
}

pub fn uint16_array(arr []u16) JS.Uint16Array {
	#let tmp = new Array();

	for elem in arr {
		_ := elem
		#tmp.push(elem.val)
	}
	mut uint_arr := JS.Uint16Array{}
	#uint_arr = new Uint16Array(tmp)

	return uint_arr
}

pub fn uint32_array(arr []u32) JS.Uint32Array {
	#let tmp = new Array();

	for elem in arr {
		_ := elem
		#tmp.push(elem.val)
	}
	mut uint_arr := JS.Uint32Array{}
	#uint_arr = new Uint32Array(tmp)

	return uint_arr
}

pub fn int8_array(arr []i8) JS.Int8Array {
	#let tmp = new Array();

	for elem in arr {
		_ := elem
		#tmp.push(elem.val)
	}
	mut int_arr := JS.Int8Array{}
	#int_arr = new Int8Array(tmp)

	return int_arr
}

pub fn int16_array(arr []i16) JS.Int16Array {
	#let tmp = new Array();

	for elem in arr {
		_ := elem
		#tmp.push(elem.val)
	}
	mut int_arr := JS.Int16Array{}
	#int_arr = new Int16Array(tmp)

	return int_arr
}

pub fn int32_array(arr []int) JS.Int32Array {
	#let tmp = new Array();

	for elem in arr {
		_ := elem
		#tmp.push(elem.val)
	}
	mut int_arr := JS.Int32Array{}
	#int_arr = new Int32Array(tmp)

	return int_arr
}

pub fn int64_array(arr []i64) JS.BigInt64Array {
	#let tmp = new Array();

	for elem in arr {
		_ := elem
		#tmp.push(elem.val)
	}
	mut int_arr := JS.BigInt64Array{}
	#int_arr = new BigInt64Array(tmp)

	return int_arr
}

pub fn uint64_array(arr []u64) JS.BigUint64Array {
	#let tmp = new Array();

	for elem in arr {
		_ := elem
		#tmp.push(elem.val)
	}
	mut int_arr := JS.BigUint64Array{}
	#int_arr = new BigUint64Array(tmp)

	return int_arr
}

pub fn float32_array(arr []f32) JS.Float32Array {
	#let tmp = new Array();

	for elem in arr {
		_ := elem
		#tmp.push(elem.val)
	}
	mut float_arr := JS.Float32Array{}
	#float_arr = new Float32Array(tmp)

	return float_arr
}

pub fn float64_array(arr []f64) JS.Float64Array {
	#let tmp = new Array();

	for elem in arr {
		_ := elem
		#tmp.push(elem.val)
	}
	mut float_arr := JS.Float64Array{}
	#float_arr = new Float64Array(tmp)

	return float_arr
}
