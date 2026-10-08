import assert from 'node:assert/strict';
import test from 'node:test';
import { runWasm } from './runtime.mjs';

// Build small guest programs directly, so the runtime tests need no V build or npm dependencies.
function leb(value, signed = false) {
	const bytes = [];
	for (;;) {
		const byte = value & 127;
		value = signed ? value >> 7 : value >>> 7;
		const done = signed ? (value === 0 && !(byte & 64)) || (value === -1 && (byte & 64)) : value === 0;
		bytes.push(byte | (done ? 0 : 128));
		if (done) return bytes;
	}
}

const name = (value) => [...leb(value.length), ...new TextEncoder().encode(value)];
const section = (id, bytes) => [id, ...leb(bytes.length), ...bytes];
const i32 = (value) => [0x41, ...leb(value, true)];
const requireEqual = (value) => [...i32(value), 0x47, 0x04, 0x40, 0x00, 0x0b];

function program(calls, segments, pages = 1) {
	const instructions = [];
	for (const { fd = 1, iovs = 32, count = 1, written = 4, errno = 0, byteCount } of calls) {
		instructions.push(...i32(fd), ...i32(iovs), ...i32(count), ...i32(written), 0x10, 0,
			...requireEqual(errno));
		if (byteCount !== undefined) {
			instructions.push(...i32(written), 0x28, 2, 0, ...requireEqual(byteCount));
		}
	}
	const body = [0, ...instructions, 0x0b];
	return Uint8Array.from([
		0, 97, 115, 109, 1, 0, 0, 0,
		...section(1, [2, 0x60, 4, 0x7f, 0x7f, 0x7f, 0x7f, 1, 0x7f, 0x60, 0, 0]),
		...section(2, [1, ...name('wasi_snapshot_preview1'), ...name('fd_write'), 0, 0]),
		...section(3, [1, 1]),
		...section(5, [1, 0, ...leb(pages)]),
		...section(7, [2, ...name('memory'), 2, 0, ...name('_start'), 0, 1]),
		...section(10, [1, ...leb(body.length), ...body]),
		...section(11, [segments.length, ...segments.flatMap(({ offset, bytes }) =>
			[0, ...i32(offset), 0x0b, ...leb(bytes.length), ...bytes])]),
	]);
}

function words(...values) {
	const bytes = new Uint8Array(values.length * 4);
	const view = new DataView(bytes.buffer);
	values.forEach((value, index) => view.setUint32(index * 4, value, true));
	return bytes;
}

async function outputOf(bytes) {
	let output = '';
	await runWasm(bytes, (text) => { output += text; });
	return output;
}

test('writes every iovec and reports the byte count', async () => {
	const bytes = program([{ count: 2, byteCount: 12 }], [
		{ offset: 32, bytes: words(128, 6, 134, 6) },
		{ offset: 128, bytes: new TextEncoder().encode('Hello world!') },
	]);
	assert.equal(await outputOf(bytes), 'Hello world!');
});

test('preserves UTF-8 split across iovecs and writes', async () => {
	const bytes = program([{ count: 2, byteCount: 3 }, { iovs: 48, byteCount: 2 }], [
		{ offset: 32, bytes: words(128, 1, 129, 2, 131, 2) },
		{ offset: 128, bytes: new TextEncoder().encode('🦀\n') },
	]);
	assert.equal(await outputOf(bytes), '🦀\n');
});

test('keeps stdout and stderr UTF-8 decoders independent', async () => {
	const bytes = program([{ byteCount: 1 }, { fd: 2, iovs: 40, byteCount: 1 },
		{ iovs: 48, byteCount: 3 }], [
		{ offset: 32, bytes: words(128, 1, 132, 1, 129, 3) },
		{ offset: 128, bytes: new TextEncoder().encode('🦀!') },
	]);
	assert.equal(await outputOf(bytes), '!🦀');
});

test('flushes an incomplete UTF-8 sequence at program exit', async () => {
	const bytes = program([{ byteCount: 1 }], [
		{ offset: 32, bytes: words(128, 1) },
		{ offset: 128, bytes: Uint8Array.of(0xf0) },
	]);
	assert.equal(await outputOf(bytes), '\ufffd');
});

test('accepts a zero-length write', async () => {
	assert.equal(await outputOf(program([{ count: 0, byteCount: 0 }], [])), '');
});

test('returns EBADF for unsupported file descriptors without changing the byte count', async () => {
	const bytes = program([{ fd: 0, errno: 8, byteCount: 17 }], [
		{ offset: 4, bytes: words(17) },
	]);
	assert.equal(await outputOf(bytes), '');
});

test('returns EFAULT for invalid iovec, data, and byte-count pointers', async () => {
	for (const call of [
		{ iovs: 65532 },
		{ iovs: -1 },
		{ count: 0x40000000 },
		{ written: 65534 },
		{},
	]) {
		const bytes = program([{ ...call, errno: 21 }], [
			{ offset: 32, bytes: words(65536, 1) },
		]);
		assert.equal(await outputOf(bytes), '');
	}
});

test('validates all iovecs before emitting output', async () => {
	const bytes = program([{ count: 2, errno: 21, byteCount: 17 }], [
		{ offset: 4, bytes: words(17) },
		{ offset: 32, bytes: words(128, 2, 65536, 1) },
		{ offset: 128, bytes: new TextEncoder().encode('OK') },
	]);
	assert.equal(await outputOf(bytes), '');
});

test('limits cumulative program output to 1 MiB', async () => {
	const bytes = program([{ byteCount: 1024 * 1024 }, { iovs: 40 }], [
		{ offset: 32, bytes: words(128, 1024 * 1024, 128, 1) },
	], 17);
	await assert.rejects(outputOf(bytes), /Output exceeded 1 MiB/);
});

test('requires the WASI memory and entry point exports', async () => {
	await assert.rejects(outputOf(Uint8Array.of(0, 97, 115, 109, 1, 0, 0, 0)),
		/must export memory and a _start function/);
});
