module js

// TODO: Fix msvc bug that's preventing $embed_file('fast_deep_equal.js')
const (
	fast_deep_eq_fn = "// https://www.npmjs.com/package/fast-deep-equal - 3/3/2021
const envHasBigInt64Array = typeof BigInt64Array !== 'undefined';
function vEq(a, b) {
	if (a === b) return true;

	if (a && b && typeof a == 'object' && typeof b == 'object') {
		if (a.constructor !== b.constructor) return false;

		var length, i, keys;
		if (Array.isArray(a)) {
			length = a.length;
			if (length != b.length) return false;
			for (i = length; i-- !== 0;)
				if (!vEq(a[i], b[i])) return false;
			return true;
		}


		if ((a instanceof Map) && (b instanceof Map)) {
			if (a.size !== b.size) return false;
			for (i of a.entries())
				if (!b.has(i[0])) return false;
			for (i of a.entries())
				if (!vEq(i[1], b.get(i[0]))) return false;
			return true;
		}

		if ((a instanceof Set) && (b instanceof Set)) {
			if (a.size !== b.size) return false;
			for (i of a.entries())
				if (!b.has(i[0])) return false;
			return true;
		}

		if (ArrayBuffer.isView(a) && ArrayBuffer.isView(b)) {
			length = a.length;
			if (length != b.length) return false;
			for (i = length; i-- !== 0;)
				if (a[i] !== b[i]) return false;
			return true;
		}


		if (a.constructor === RegExp) return a.source === b.source && a.flags === b.flags;
		if (a.valueOf !== Object.prototype.valueOf) return a.valueOf() === b.valueOf();
		if (a.toString !== Object.prototype.toString) return a.toString() === b.toString();

		keys = Object.keys(a);
		length = keys.length;
		if (length !== Object.keys(b).length) return false;

		for (i = length; i-- !== 0;)
			if (!Object.prototype.hasOwnProperty.call(b, keys[i])) return false;

		for (i = length; i-- !== 0;) {
			var key = keys[i];

			if (!vEq(a[key], b[key])) return false;
		}

		return true;
	}

	// true if both NaN, false otherwise
	return a!==a && b!==b;
};
"
)
