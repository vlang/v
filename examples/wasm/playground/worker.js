import { runWasm } from './runtime.mjs';

const send = (type, fields = {}) => self.postMessage({ type, ...fields });

// Emscripten preload failures can reject outside the compiler factory's promise.
self.addEventListener('unhandledrejection', (event) => {
	event.preventDefault();
	const detail = event.reason?.message || String(event.reason);
	send('error', {
		message: 'Could not load the V compiler assets. Rebuild with sh examples/wasm/playground/build.sh '
			+ `and serve the playground directory over HTTP. ${detail}`,
	});
});

self.onmessage = async ({ data }) => {
	if (data.type !== 'run') return;
	try {
		send('status', { message: 'Loading the V compiler…' });
		let createCompiler;
		try {
			({ default: createCompiler } = await import('./build/compiler.mjs'));
		} catch {
			throw new Error('Could not load the V compiler. Run sh examples/wasm/playground/build.sh in the repository, then serve the playground directory over HTTP.');
		}
		const compiler = await createCompiler({
			noInitialRun: true,
			thisProgram: '/v/v',
			locateFile: (name) => new URL(`./build/${name}`, import.meta.url).href,
			print: (text) => send('output', { text: `${text}\n` }),
			printErr: (text) => send('output', { text: `${text}\n` }),
			preRun: [(module) => {
				module.ENV.VEXE = '/v/v';
				module.ENV.VJOBS = '1';
				module.ENV.V_SKIP_VVMRC = '1';
				module.ENV.V_MACOS_V3_EMBEDDED = '1';
			}],
		});
		compiler.FS.mkdirTree('/playground');
		compiler.FS.writeFile('/v/v', '');
		compiler.FS.chdir('/v');
		compiler.FS.writeFile('/playground/main.v', data.source);
		send('status', { message: 'Compiling…' });
		const result = compiler.callMain([
			'-silent', '-no-parallel', '-no-memory-limit', '-nocache',
			'-b', 'wasm', '-o', '/playground/main.wasm', '/playground/main.v',
		]);
		if (result !== 0) throw new Error(`Compilation failed (exit code ${result}).`);
		const bytes = compiler.FS.readFile('/playground/main.wasm');
		send('status', { message: 'Running…' });
		let output = '';
		const flush = () => {
			if (output) send('output', { text: output });
			output = '';
		};
		try {
			await runWasm(bytes, (text) => {
				output += text;
				if (output.includes('\n') || output.length >= 4096) flush();
			});
		} finally {
			flush();
		}
		send('done');
	} catch (error) {
		send('error', { message: error.message || String(error) });
	}
};
