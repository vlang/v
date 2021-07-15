module builtin

struct array {
	arr JS.Array
	pub: 
		len int
}

#function flatIntoArray(target, source, sourceLength, targetIndex, depth) {
#    "use strict";
#
#    for (var sourceIndex = 0; sourceIndex < sourceLength; ++sourceIndex) {
#        if (sourceIndex in source) {
#            var element = source[sourceIndex];
#            if (depth > 0 && Array.isArray(element))
#                targetIndex = flatIntoArray(target, element, element.length, targetIndex, depth - 1);
#            else {
#                target[targetIndex] = element;
#                ++targetIndex;
#            }
#        }
#    }
#    return targetIndex;
#}
#function flatArray(target,depth) {
#	 var array = target
#    var length = array.length;
#    var depthNum = 1;
#
#    if (depth !== undefined)
#        depthNum = +depth
#
#    var result = []
#
#    flatIntoArray(result, array, length, 0, depthNum);
#    return result;
#}

[unsafe]
pub fn (a array) repeat_to_depth(count int,depth int) array {
	if count < 0 {
		panic('array.repeat: count is negative: $count')
	}
	mut arr := empty_array()
	#let tmp = new Array(a.arr.length * +count);
	# tmp.fill(a.arr);
	# 
	#arr.arr = flatArray(tmp,depth+1);
	return arr
}

fn (a array) get(ix int) voidptr {
	mut result := voidptr(0)
	#result = a.arr[ix]
	return result
}


pub fn (a array) repeat(count int) array {
	unsafe {
		return a.repeat_to_depth(count,0)
	}
}

fn empty_array() array {
	mut arr := array{}
	#arr = new array([])
	return arr
}


fn (a &array) set_len(i int) {
	#a.arr.length=i
}
[unsafe]
pub fn (a &array) clone_to_depth(depth int) array {
	mut size:=a.len
	if size == 0 {
		size = 0
	}

	mut arr := array{JS.Array {},0}
	arr.set_len(size)

	if depth > 0 {
		for i in 0..a.len {
			ar := array {}
			#ar = a.arr[i]
			ar_clone := unsafe {ar.clone_to_depth(depth-1)}
			#arr.arr[i]=ar_clone
		}
		return arr
	} else {
		for i in 0..a.len {
			#arr.arr[i] = a.arr[i]
		}
		return arr
	}
}