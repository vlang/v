// Build: ./v -b wasm -o add.wasm examples/wasm/add.v
// Run: node examples/wasm/add.mjs add.wasm
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';

const module = await WebAssembly.compile(readFileSync(process.argv[2] ?? 'add.wasm'));
assert.deepEqual(WebAssembly.Module.imports(module), []);
const instance = await WebAssembly.instantiate(module, {});
assert.equal(instance.exports.add(20, 22), 42);
assert.equal(instance.exports.add(-7, 2), -5);
console.log(instance.exports.add(20, 22));
