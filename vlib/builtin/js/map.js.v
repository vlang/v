module builtin

struct map {
	m JS.Map
pub:
	len int
}

fn (mut m map) internal_set(key JS.Any, val JS.Any) {
	//$if es5 {
	#if ('$toJS' in key) key = key.$toJS();
	#if (!(key in m.val.map)) m.val.length++;
	#m.val.map[key] = val
	/*} $else {
		# if ('$toJS' in key) key = key.$toJS();
		# m.val.m.set(key,val);
	}*/
	_ := key
	_ := val
}

fn (mut m map) internal_get(key JS.Any) JS.Any {
	mut val := JS.Any(unsafe { nil })
	//$if es5 {
	#if (typeof key != "string" && '$toJS' in key) key = key.$toJS();
	#val =  m.val.map[key]
	/*} $else {
		# if ('$toJS' in key) key = key.$toJS();
		# val = m.val.m.get(key)
	}*/
	_ := key
	return val
}

#map.prototype.get = function (key) { return map_internal_get(this,key); }
#map.prototype.set = function(key,val) { map_internal_set(this,key,val); }
#map.prototype.has = function (key) { if (typeof key != "string" && '$toJS' in key) { key = key.$toJS() } return key in this.map; }
// Removes the mapping of a particular key from the map.
[unsafe]
pub fn (mut m map) delete(key JS.Any) {
	#let k = '$toJS' in key ? key.$toJS() : key;

	#if (delete m.val.map[k]) { m.val.length--; };

	_ := key
}

pub fn (m &map) free() {}

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

#map.prototype.getOrSet = function (key, init) { if (this.map.has(key)) { return this.map.get(key); } else { this.map.set(key,init); return init; } }
