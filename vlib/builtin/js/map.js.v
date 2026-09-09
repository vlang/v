module builtin

struct map {
	m JS.Map
pub:
	len int
}

fn (mut m map) internal_set(key JS.Any, val JS.Any) {
	//$if es5 {
	#let skey = key;
	#if (key != null && typeof key.$toJS === 'function') key = key.$toJS();
	#if (!(key in m.val.map)) {
	#m.val.length++;
	#m.val.map[key] = { key: skey, val: val };
	#} else {
	#m.val.map[key].val = val
	#}
	/*} $else {
		# if (key.hasOwnProperty('$toJS')) key = key.$toJS();
		# m.val.m.set(key,val);
	}*/
	_ := key
	_ := val
}

fn (mut m map) internal_get(key JS.Any) JS.Any {
	mut val := JS.Any(unsafe { nil })
	//$if es5 {
	#if (typeof key != "string" && key != null && typeof key.$toJS === 'function') key = key.$toJS();
	#if (key in m.val.map) {
	#val = m.val.map[key].val
	#} else {
	#val = js_undefined()
	#}
	/*} $else {
		# if (key.hasOwnProperty('$toJS')) key = key.$toJS();
		# val = m.val.m.get(key)
	}*/
	_ := key
	return val
}

#map.prototype.get = function (key) { return map_internal_get(this,key); }
#map.prototype.set = function(key,val) { map_internal_set(this,key,val); }
#map.prototype.has = function (key) { if (typeof key != "string" && key != null && typeof key.$toJS === 'function') { key = key.$toJS() } return key in this.map; }
// Removes the mapping of a particular key from the map.
@[unsafe]
pub fn (mut m map) delete(key JS.Any) {
	#let k = key != null && typeof key.$toJS === 'function' ? key.$toJS() : key;

	#if (k in m.val.map) {
	#delete m.val.map[k];
	#m.val.length--;
	#}

	_ := key
}

pub fn (m map) clone() map {
	mut res := m
	#res = v_clone_value(m)

	return res
}

pub fn (m &map) free() {}

pub fn (m map) keys() array {
	ret := JS.makeEmptyArray()
	#for (var key in m.map) array_push(ret,m.map[key].key,false)

	return ret
}

pub fn (m map) values() array {
	ret := JS.makeEmptyArray()
	#for (var key in m.map) array_push(ret,m.map[key].val,false);

	return ret
}

//#Object.defineProperty(map.prototype,"len",{get: function() { return this.map.size; }})
#map.prototype.toString = function () {
#function fmtKey(key) { return typeof key == 'string' ? '\'' + key + '\'' : key}
#let res = '{'
#for (const entry of this) {
#res += fmtKey(entry[0]) + ': ' + entry[0];
#}
#res += '}'
#return res;
#}

#map.prototype.getOrSet = function (key, init) { const skey = key; if (typeof key != "string" && key != null && typeof key.$toJS === 'function') { key = key.$toJS() } if (key in this.map) { return this.map[key].val; } this.length++; this.map[key] = { key: skey, val: init }; return this.map[key].val; }
