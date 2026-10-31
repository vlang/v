import assert from 'node:assert/strict';
import { mkdtemp, mkdir, copyFile, writeFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import test from 'node:test';
import { Worker } from 'node:worker_threads';

// This real WASI guest writes "reached\n" once, then executes an infinite loop.
// Only the compiler factory is replaced; worker output and runWasm remain real.
const guest = 'AGFzbQEAAAABDAJgBH9/f38Bf2AAAAIjARZ3YXNpX3NuYXBzaG90X3ByZXZpZXcx'
	+ 'CGZkX3dyaXRlAAADAgEBBQMBAAEHEwIGbWVtb3J5AgAGX3N0YXJ0AAEKFAESAEEBQSBBAUEE'
	+ 'EAAaA0AMAAsLCxwCAEEgCwiAAAAACAAAAABBgAELCHJlYWNoZWQK';

test('shows a short output line before an infinite guest is stopped', { timeout: 10000 }, async (t) => {
	assert.ok(WebAssembly.validate(Buffer.from(guest, 'base64')));
	const root = await mkdtemp(join(tmpdir(), 'v-playground-output-'));
	let worker;
	t.after(async () => {
		if (worker) await worker.terminate();
		await rm(root, { recursive: true, force: true });
	});
	await mkdir(join(root, 'build'));
	await writeFile(join(root, 'package.json'), '{"type":"module"}\n');
	for (const file of ['worker.js', 'runtime.mjs']) {
		await copyFile(new URL(file, import.meta.url), join(root, file));
	}
	await writeFile(join(root, 'build', 'compiler.mjs'), `
export default async () => ({
	FS: {
		mkdirTree() {}, writeFile() {}, chdir() {},
		readFile() { return Uint8Array.from(Buffer.from('${guest}', 'base64')); },
	},
	callMain() { return 0; },
});
`);
	await writeFile(join(root, 'bridge.mjs'), `
import { parentPort } from 'node:worker_threads';
globalThis.self = {
	postMessage(message) { parentPort.postMessage(message); },
	addEventListener() {},
};
await import('./worker.js');
parentPort.on('message', (data) => self.onmessage({ data }));
`);
	worker = new Worker(join(root, 'bridge.mjs'));
	const messages = [];
	let output = '';
	let timer;
	try {
		await new Promise((resolve, reject) => {
			timer = setTimeout(() => reject(new Error(
				`No output received from the running guest: ${JSON.stringify(messages)}`,
			)), 5000);
			worker.once('error', reject);
			worker.on('message', (message) => {
				messages.push(message);
				if (message.type === 'error') reject(new Error(message.message));
				if (message.type === 'output') {
					output += message.text;
					if (output.includes('reached\n')) resolve();
				}
			});
			worker.postMessage({ type: 'run', source: "fn main() { println('reached'); for {} }" });
		});
	} finally {
		clearTimeout(timer);
	}
	assert.equal(output, 'reached\n');
	assert.ok(messages.some((message) => message.type === 'status' && message.message === 'Running…'));
	assert.ok(!messages.some((message) => message.type === 'done'));
	await worker.terminate();
	worker = null;
	assert.equal(output, 'reached\n');
});
