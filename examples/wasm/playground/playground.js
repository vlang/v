const source = document.getElementById('source');
const output = document.getElementById('output');
const status = document.getElementById('status');
const example = document.getElementById('example');
const run = document.getElementById('run');
const stop = document.getElementById('stop');

const examples = {
	hello: source.value,
	squares: `fn main() {
	for number in 1 .. 6 {
		println(number * number)
	}
}
`,
	fibonacci: `fn fibonacci(n int) int {
	if n < 2 {
		return n
	}
	return fibonacci(n - 1) + fibonacci(n - 2)
}

fn main() {
	for number in 0 .. 12 {
		println(fibonacci(number))
	}
}
`,
};

let worker = null;

function finish(message) {
	if (worker) {
		worker.terminate();
		worker = null;
	}
	run.disabled = false;
	stop.disabled = true;
	example.disabled = false;
	status.textContent = message;
}

function fail(message) {
	output.textContent += `${output.textContent && !output.textContent.endsWith('\n') ? '\n' : ''}${message}\n`;
	finish('Failed. See the output for details.');
}

function runSource() {
	if (worker) return;
	if (!source.value.trim()) {
		status.textContent = 'Write a V program before running.';
		source.focus();
		return;
	}
	output.textContent = '';
	run.disabled = true;
	stop.disabled = false;
	example.disabled = true;
	status.textContent = 'Loading the V compiler…';
	try {
		const current = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' });
		worker = current;
		current.onmessage = ({ data }) => {
			if (worker !== current) return;
			switch (data.type) {
				case 'status':
					status.textContent = data.message;
					break;
				case 'output':
					output.textContent += data.text;
					output.scrollTop = output.scrollHeight;
					break;
				case 'done':
					finish('Finished.');
					break;
				case 'error':
					fail(data.message);
					break;
			}
		};
		current.onerror = (event) => {
			if (worker !== current) return;
			event.preventDefault();
			fail(event.message || 'Could not load the playground worker. Serve this directory over HTTP and try again.');
		};
		current.onmessageerror = () => {
			if (worker === current) fail('Could not read a response from the playground worker.');
		};
		current.postMessage({ type: 'run', source: source.value });
	} catch (error) {
		fail(`Could not start the playground: ${error.message}. Serve this directory over HTTP and try again.`);
	}
}

example.addEventListener('change', () => {
	source.value = examples[example.value];
});
run.addEventListener('click', runSource);
stop.addEventListener('click', () => finish('Stopped.'));
source.addEventListener('keydown', (event) => {
	if (event.key === 'Enter' && (event.ctrlKey || event.metaKey)) {
		event.preventDefault();
		runSource();
	}
});
