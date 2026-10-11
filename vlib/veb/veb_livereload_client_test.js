// Run with: node --test vlib/veb/veb_livereload_client_test.js
const assert = require("node:assert/strict");
const { readFileSync } = require("node:fs");
const { test } = require("node:test");
const { runInNewContext } = require("node:vm");

const source = readFileSync(__dirname + "/veb_livereload.js", "utf8");

function element(name, html, attrs = {}) {
	return {
		nodeType: 1, nodeName: name, outerHTML: html,
		hasAttribute: (key) => key in attrs,
		getAttribute: (key) => attrs[key],
		setAttribute(key, value) { attrs[key] = value; },
		cloneNode() { return element(name, html, { ...attrs }); },
	};
}

function page(scripts = [], head = []) {
	return {
		title: "Example", head: { children: head },
		querySelectorAll: (selector) => selector === "script" ? scripts : [],
		body: { attributes: [], children: [], childNodes: [], firstChild: null },
	};
}

function client(oldPage, newPage) {
	const handlers = {};
	let reloads = 0;
	let fetches = 0;
	oldPage.currentScript = { dataset: { started: "1", toast: "0" } };
	runInNewContext(source, {
		document: oldPage,
		window: { location: { href: "http://localhost/", origin: "http://localhost", reload() { reloads++; } } },
		console: { debug() {} }, URL, setTimeout, clearTimeout,
		EventSource: class { addEventListener(event, handler) { handlers[event] = handler; } },
		DOMParser: class { parseFromString() { return newPage; } },
		fetch: async () => { fetches++; return { ok: true, headers: { get: () => "text/html" }, text: async () => "page" }; },
	});
	return {
		async change(kind) { handlers.change({ data: kind }); await new Promise(setImmediate); },
		get reloads() { return reloads; }, get fetches() { return fetches; },
	};
}

test("non-CSS static assets navigate so changed scripts are executed", async () => {
	const c = client(page(), page());
	await c.change("full");
	assert.equal(c.reloads, 1);
	assert.equal(c.fetches, 0);
});

test("template refresh navigates when inline scripts change", async () => {
	const c = client(page([element("SCRIPT", "<script>one()</script>")]), page([element("SCRIPT", "<script>two()</script>")]));
	await c.change("page");
	assert.equal(c.reloads, 1);
});

test("template refresh navigates when head resources change", async () => {
	const c = client(page([], [element("STYLE", "<style>body{color:red}</style>")]), page([], [element("STYLE", "<style>body{color:blue}</style>")]));
	await c.change("page");
	assert.equal(c.reloads, 1);
});

test("unchanged resources morph without navigation and ignore the injected script", async () => {
	const c = client(page([element("SCRIPT", "<script>one()</script>"), element("SCRIPT", "old livereload", { "data-veb-livereload": "" })]), page([element("SCRIPT", "<script>one()</script>"), element("SCRIPT", "new livereload", { "data-veb-livereload": "" })]));
	await c.change("page");
	assert.equal(c.fetches, 1);
	assert.equal(c.reloads, 0);
});
