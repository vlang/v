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

// This small DOM model exercises node reuse and field values, including sibling
// insertion. Resource comparisons above need only immutable element markup.
class FormElement {
	constructor(nodeName, attrs = {}, value = "") {
		this.nodeType = 1; this.nodeName = nodeName; this.attrs = { ...attrs };
		this.value = value; this.childNodes = [];
	}
	get attributes() { return Object.entries(this.attrs).map(([name, value]) => ({ name, value })); }
	get children() { return this.childNodes; }
	get firstChild() { return this.childNodes[0] || null; }
	get id() { return this.attrs.id || ""; }
	get nextSibling() { return this.parentNode?.childNodes[this.parentNode.childNodes.indexOf(this) + 1] || null; }
	hasAttribute(name) { return name in this.attrs; }
	getAttribute(name) { return this.attrs[name] ?? null; }
	setAttribute(name, value) { this.attrs[name] = value; }
	removeAttribute(name) { delete this.attrs[name]; }
	insertBefore(child, cursor) {
		child.remove();
		const index = cursor ? this.childNodes.indexOf(cursor) : this.childNodes.length;
		this.childNodes.splice(index, 0, child); child.parentNode = this;
	}
	remove() {
		if (this.parentNode) {
			const siblings = this.parentNode.childNodes;
			siblings.splice(siblings.indexOf(this), 1); this.parentNode = null;
		}
	}
	cloneNode() { return new FormElement(this.nodeName, this.attrs); }
}

function formPage(inputs) {
	const doc = page();
	doc.body = new FormElement("BODY");
	for (const input of inputs) doc.body.insertBefore(input, null);
	doc.importNode = (node) => node.cloneNode(true);
	return doc;
}

test("inserting a named field keeps typed input attached to the original field", async () => {
	const username = new FormElement("INPUT", { name: "username", type: "text" }, "Alex");
	const oldPage = formPage([username]);
	const newPage = formPage([new FormElement("INPUT", { name: "email", type: "text" }), new FormElement("INPUT", { name: "username", type: "text" })]);
	const c = client(oldPage, newPage);
	await c.change("page");
	assert.equal(c.reloads, 0);
	assert.equal(oldPage.body.children[0].getAttribute("name"), "email");
	assert.equal(oldPage.body.children[0].value, "");
	assert.equal(oldPage.body.children[1], username);
	assert.equal(username.value, "Alex");
});

test("a keyed field with a changed name or type does not inherit a typed value", async () => {
	for (const attrs of [{ id: "field", name: "email", type: "text" }, { id: "field", name: "username", type: "password" }]) {
		const oldPage = formPage([new FormElement("INPUT", { id: "field", name: "username", type: "text" }, "Alex")]);
		const c = client(oldPage, formPage([new FormElement("INPUT", attrs)]));
		await c.change("page");
		assert.equal(c.reloads, 0);
		assert.equal(oldPage.body.children[0].value, "");
	}
});
