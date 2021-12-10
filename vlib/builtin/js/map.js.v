module builtin

struct map {
	m JS.Map
pub:
	len int
}

// Removes the mapping of a particular key from the map.
[unsafe]
pub fn (mut m map) delete(key voidptr) {
	#m.map.delete(key)
}

pub fn (m &map) free() {}

#map.prototype[Symbol.iterator] = function () { return this.map[Symbol.iterator](); }
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
