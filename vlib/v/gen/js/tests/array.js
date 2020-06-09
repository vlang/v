// V_COMMIT_HASH 808975f
// V_CURRENT_COMMIT_HASH 564545d
// Generated by the V compiler

"use strict";

/** @namespace builtin */
const builtin = (function () {
	/**
	 * @function
	 * @param {any} s
	 * @returns {void}
	*/
	function println(s) {
		console.log(s);
	}
	
	/**
	 * @function
	 * @param {any} s
	 * @returns {void}
	*/
	function print(s) {
		process.stdout.write(s);
	}

	/* module exports */
	return {
		println,
		print
	};
})();

/** @namespace main */
const main = (function () {
	/**
	 * @function
	 * @param {...number} args
	 * @returns {void}
	*/
	function variadic(...args) {
		builtin.println(args);
		builtin.println(args[0]);
		builtin.println(args[1]);
	}
	
	/**
	 * @function
	 * @returns {void}
	*/
	function vararg_test() {
		variadic(1, 2, 3);
	}
	
	/* program entry point */
	(function() {
		vararg_test();
		/** @type {string[]} */
		const arr1 = ["Hello", "JS", "Backend"];
		/** @type {number[]} */
		let arr2 = [1, 2, 3, 4, 5];
		/** @type {string[]} */
		const slice1 = arr1.slice(1, 3);
		/** @type {number[]} */
		const slice2 = arr2.slice(0, 3);
		/** @type {number[]} */
		const slice3 = arr2.slice(3, arr2.length);
		/** @type {string} */
		const idx1 = slice1[1];
		arr2[0] = 1;
		arr2[0 + 1] = 2;
		builtin.println(arr2);
		arr2.push(6);
		arr2.push(...[7, 8, 9]);
		builtin.println(arr2);
		builtin.println("\n\n");
		/** @type {string} */
		let slice4 = idx1.slice(0, 4);
		builtin.print("Back\t=> ");
		builtin.println(slice4);
		/** @type {number} */
		const idx2 = "😀".charCodeAt(0);
		builtin.print("66\t=> ");
		builtin.println(idx2);
		/** @type {Map<string, string>} */
		let m = new Map();
		/** @type {string} */
		const key = "key";
		m.set(key, "value");
		/** @type {string} */
		const val = m.get("key");
		builtin.print("value\t=> ");
		builtin.println(val);
		builtin.print("true\t=> ");
		builtin.println(arr1.includes("JS"));
		builtin.print("false\t=> ");
		builtin.println(!(arr2.includes(3)));
		builtin.print("true\t=> ");
		builtin.println(m.has("key"));
		builtin.print("true\t=> ");
		builtin.println(!(m.has("badkey")));
		for (let _tmp1 = 0; _tmp1 < arr1.length; ++_tmp1) {
		}
		
		builtin.println("0 to 8\t=>");
		for (let i = 0; i < arr2.length; ++i) {
			builtin.println(i);
		}
		
		builtin.println("\n\n4 to 5\t=> ");
		for (let _tmp2 = 0; _tmp2 < slice3.length; ++_tmp2) {
			const v = slice3[_tmp2];
			builtin.println(v);
		}
		
	})();

	/* module exports */
	return {};
})();


