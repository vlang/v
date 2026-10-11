// The current V wasm backend imports only WASI fd_write for stdout and stderr.
export async function runWasm(bytes, onOutput) {
	const decoders = new Map([[1, new TextDecoder()], [2, new TextDecoder()]]);
	let instance;
	let outputBytes = 0;
	const imports = {
		wasi_snapshot_preview1: {
			fd_write(fd, iovs, count, written) {
				if (!decoders.has(fd)) return 8; // WASI EBADF
				const memory = instance.exports.memory.buffer;
				const view = new DataView(memory);
				// Reject invalid guest pointers with WASI EFAULT before writing output.
				const inBounds = (ptr, size) => ptr <= memory.byteLength && size <= memory.byteLength - ptr;
				iovs >>>= 0;
				count >>>= 0;
				written >>>= 0;
				if (!inBounds(iovs, count * 8) || !inBounds(written, 4)) return 21;
				const chunks = [];
				let total = 0;
				for (let i = 0; i < count; i++) {
					const ptr = view.getUint32(iovs + i * 8, true);
					const length = view.getUint32(iovs + i * 8 + 4, true);
					if (!inBounds(ptr, length)) return 21;
					total += length;
					chunks.push(new Uint8Array(memory, ptr, length));
				}
				outputBytes += total;
				if (outputBytes > 1024 * 1024) throw new Error('Output exceeded 1 MiB. Stop or shorten the program.');
				for (const chunk of chunks) {
					const text = decoders.get(fd).decode(chunk, { stream: true });
					if (text) onOutput(text);
				}
				view.setUint32(written, total, true);
				return 0;
			},
		},
	};
	({ instance } = await WebAssembly.instantiate(bytes, imports));
	if (!(instance.exports.memory instanceof WebAssembly.Memory) || typeof instance.exports._start !== 'function') {
		throw new Error('The compiled program must export memory and a _start function.');
	}
	instance.exports._start();
	for (const decoder of decoders.values()) {
		const text = decoder.decode();
		if (text) onOutput(text);
	}
}
