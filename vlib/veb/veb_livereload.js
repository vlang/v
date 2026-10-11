"use strict";
// veb livereload client, injected into HTML pages when the app is compiled with
// `-d veb_livereload`. It keeps one Server-Sent Events stream open, and applies
// changes in place: a server restart or a page change morphs the DOM (keeping
// scroll position, focus and typed form values), a stylesheet change swaps the
// stylesheet without reloading.
(() => {
	if (window.__veb_livereload) {
		return;
	}
	window.__veb_livereload = true;
	const me = document.currentScript;
	const started = me.dataset.started;
	const retryMax = Math.max(Number(me.dataset.retryMax) || 250, 1);
	const retryFirst = Math.min(250, retryMax);
	const method = (me.dataset.method || "GET").toUpperCase();
	const log = (...args) => console.debug("[veb livereload]", ...args);
	let serverId = started;
	let busy = false;
	let again = false;

	const isOurs = (node) => node.nodeType === 1 && node.hasAttribute("data-veb-livereload");

	// ui shows a small status toast, and a panel with the compiler output when a
	// build fails. It lives in a shadow root on <html>, outside <body>, so a morph
	// never touches it and the page's CSS cannot restyle it.
	const ui = (() => {
		if (me.dataset.toast === "0") {
			return { toast() {}, hideToast() {}, showError() {}, hideError() {} };
		}
		const host = document.createElement("div");
		host.setAttribute("data-veb-livereload", "");
		const root = host.attachShadow({ mode: "open" });
		root.innerHTML = `<style>
			:host { all: initial; }
			.toast, .panel { position: fixed; z-index: 2147483647; font: 13px/1.4 system-ui, sans-serif; color: #f4f4f5; background: #18181b; box-shadow: 0 4px 16px rgba(0,0,0,.3); }
			.toast { right: 16px; bottom: 16px; padding: 8px 12px; border-radius: 8px; opacity: 0; transform: translateY(8px); transition: opacity .15s, transform .15s; pointer-events: none; }
			.toast.on { opacity: 1; transform: none; }
			.toast.busy::before { content: ""; display: inline-block; width: 8px; height: 8px; margin-right: 8px; border-radius: 50%; background: #facc15; animation: pulse 1s infinite; }
			@keyframes pulse { 50% { opacity: .3; } }
			.panel { left: 16px; right: 16px; bottom: 16px; max-height: 50vh; display: none; flex-direction: column; border-radius: 8px; border-left: 4px solid #ef4444; }
			.panel.on { display: flex; }
			.head { display: flex; align-items: center; justify-content: space-between; padding: 8px 12px; font-weight: 600; }
			button { all: unset; cursor: pointer; padding: 0 6px; font-size: 18px; color: #a1a1aa; }
			pre { margin: 0; padding: 0 12px 12px; overflow: auto; font: 12px/1.5 ui-monospace, monospace; white-space: pre-wrap; }
		</style>
		<div class="toast" role="status" aria-live="polite"></div>
		<div class="panel" role="alert"><div class="head"><span>Build failed: the previous version is still running</span><button aria-label="Dismiss">&times;</button></div><pre></pre></div>`;
		const toastEl = root.querySelector(".toast");
		const panelEl = root.querySelector(".panel");
		root.querySelector("button").addEventListener("click", () => panelEl.classList.remove("on"));
		const mount = () => document.documentElement.appendChild(host);
		if (document.body) {
			mount();
		} else {
			document.addEventListener("DOMContentLoaded", mount, { once: true });
		}
		let hideTimer = 0;
		return {
			// toast shows `text`; `busy` keeps it up with an activity dot until replaced.
			toast(text, busy = false) {
				clearTimeout(hideTimer);
				toastEl.textContent = text;
				toastEl.classList.toggle("busy", busy);
				toastEl.classList.add("on");
				if (!busy) {
					hideTimer = setTimeout(() => toastEl.classList.remove("on"), 1500);
				}
			},
			hideToast() {
				clearTimeout(hideTimer);
				toastEl.classList.remove("on");
			},
			showError(text) {
				panelEl.querySelector("pre").textContent = text;
				panelEl.classList.add("on");
			},
			hideError() {
				panelEl.classList.remove("on");
			},
		};
	})();

	const reload = () => window.location.reload();

	// refreshStylesheets re-requests every same-origin stylesheet, and removes the
	// old <link> only once the new one has loaded, so the page never flashes unstyled.
	const refreshStylesheets = () => {
		for (const old of document.querySelectorAll('link[rel~="stylesheet"]')) {
			const url = new URL(old.href, window.location.href);
			if (url.origin !== window.location.origin) {
				continue;
			}
			url.searchParams.set("veb_livereload", Date.now().toString());
			const fresh = old.cloneNode();
			fresh.href = url.href;
			fresh.addEventListener("load", () => old.remove(), { once: true });
			fresh.addEventListener("error", () => fresh.remove(), { once: true });
			old.after(fresh);
		}
		ui.toast("Styles updated");
	};

	const sameKind = (a, b) => {
		if (a.nodeType !== b.nodeType || a.nodeName !== b.nodeName) {
			return false;
		}
		if (a.nodeType !== 1) {
			return true;
		}
		if ((a.id || "") !== (b.id || "")) {
			return false;
		}
		// Typed values belong to a particular field. Reusing a node for a newly
		// inserted field with a different name would move the user's input to it.
		if (["INPUT", "TEXTAREA", "SELECT"].includes(a.nodeName)) {
			if ((a.getAttribute("name") || "") !== (b.getAttribute("name") || "")) {
				return false;
			}
			if (a.nodeName === "INPUT" && (a.getAttribute("type") || "text").toLowerCase() !== (b.getAttribute("type") || "text").toLowerCase()) {
				return false;
			}
		}
		return true;
	};

	const syncAttributes = (oldEl, newEl) => {
		for (const attr of [...oldEl.attributes]) {
			if (!newEl.hasAttribute(attr.name)) {
				oldEl.removeAttribute(attr.name);
			}
		}
		for (const attr of newEl.attributes) {
			if (oldEl.getAttribute(attr.name) !== attr.value) {
				oldEl.setAttribute(attr.name, attr.value);
			}
		}
	};

	// morphNode updates `oldNode` in place to match `newNode`. Form controls keep
	// what the user typed: only their attributes (and so their defaults) change.
	const morphNode = (oldNode, newNode) => {
		if (oldNode.nodeType === 3 || oldNode.nodeType === 8) {
			if (oldNode.nodeValue !== newNode.nodeValue) {
				oldNode.nodeValue = newNode.nodeValue;
			}
			return;
		}
		if (oldNode.nodeType !== 1) {
			return;
		}
		syncAttributes(oldNode, newNode);
		switch (oldNode.nodeName) {
			case "INPUT":
				return;
			case "TEXTAREA":
				if (oldNode.value === oldNode.defaultValue) {
					oldNode.defaultValue = newNode.defaultValue;
				}
				return;
			case "SCRIPT":
				// Not re-run: a script inserted by a morph never executes.
				if (oldNode.textContent !== newNode.textContent) {
					oldNode.textContent = newNode.textContent;
				}
				return;
		}
		morphChildren(oldNode, newNode);
	};

	// morphChildren makes the children of `oldParent` match those of `newParent`,
	// reusing existing nodes: by `id` first, then by node type and tag in order,
	// looking a few siblings ahead past inserted or removed nodes.
	const morphChildren = (oldParent, newParent) => {
		const byId = new Map();
		for (const child of oldParent.children) {
			if (child.id) {
				byId.set(child.id, child);
			}
		}
		let cursor = oldParent.firstChild;
		const skipOurs = () => {
			while (cursor && isOurs(cursor)) {
				cursor = cursor.nextSibling;
			}
		};
		for (const next of [...newParent.childNodes]) {
			if (isOurs(next)) {
				continue;
			}
			skipOurs();
			let match = null;
			if (next.nodeType === 1 && next.id) {
				const found = byId.get(next.id);
				if (found && sameKind(found, next)) {
					match = found;
					byId.delete(next.id);
				}
			} else if (cursor && !(cursor.nodeType === 1 && cursor.id) && sameKind(cursor, next)) {
				match = cursor;
			} else if (next.nodeType === 1) {
				// Look a few siblings ahead for the same element, so that an inserted
				// sibling does not cause everything after it to be rebuilt. Skipped
				// nodes stay in place: they may match a later new node.
				let probe = cursor ? cursor.nextSibling : null;
				for (let i = 0; probe && i < 8; i++, probe = probe.nextSibling) {
					if (!isOurs(probe) && !probe.id && sameKind(probe, next)) {
						match = probe;
						break;
					}
				}
			}
			if (match) {
				if (match === cursor) {
					cursor = cursor.nextSibling;
				} else {
					oldParent.insertBefore(match, cursor);
				}
				morphNode(match, next);
			} else {
				oldParent.insertBefore(document.importNode(next, true), cursor);
			}
		}
		while (cursor) {
			const after = cursor.nextSibling;
			if (!isOurs(cursor)) {
				cursor.remove();
			}
			cursor = after;
		}
		// Elements with an id that the new page no longer has.
		for (const stale of byId.values()) {
			if (stale.parentNode === oldParent) {
				stale.remove();
			}
		}
	};

	// Scripts are intentionally not executed during a morph. A changed script or
	// head element therefore requires navigation, so the browser applies it.
	const pageResources = (doc) => JSON.stringify([
		[...doc.querySelectorAll("script")].filter((node) => !isOurs(node)).map((node) => node.outerHTML),
		[...doc.head.children].filter((node) => !isOurs(node) && node.nodeName !== "TITLE" && node.nodeName !== "SCRIPT").map((node) => {
			const copy = node.cloneNode(true);
			if (copy.nodeName === "LINK" && copy.hasAttribute("href")) {
				const url = new URL(copy.getAttribute("href"), window.location.href);
				url.searchParams.delete("veb_livereload");
				copy.setAttribute("href", url.href);
			}
			return copy.outerHTML;
		}),
	]);

	// refreshPage fetches the current URL again and morphs the page into it.
	// Pages that came from a POST, failed fetches, and non-HTML answers reload.
	const refreshPage = async () => {
		if (busy) {
			again = true;
			return;
		}
		busy = true;
		try {
			if (method !== "GET") {
				reload();
				return;
			}
			const res = await fetch(window.location.href, {
				headers: { "X-Veb-Livereload": "1" },
				cache: "no-store",
				credentials: "same-origin",
			});
			const type = res.headers.get("content-type") || "";
			if (!res.ok || !type.includes("text/html")) {
				reload();
				return;
			}
			const doc = new DOMParser().parseFromString(await res.text(), "text/html");
			if (pageResources(document) !== pageResources(doc)) {
				reload();
				return;
			}
			if (document.title !== doc.title) {
				document.title = doc.title;
			}
			syncAttributes(document.body, doc.body);
			morphChildren(document.body, doc.body);
			log("page updated");
			ui.hideError();
			ui.toast("Updated");
		} catch (err) {
			log("morph failed, reloading", err);
			reload();
		} finally {
			busy = false;
			if (again) {
				again = false;
				refreshPage();
			}
		}
	};

	// connect opens the stream. When it drops (the app restarts), the browser's
	// own fixed-delay retry is replaced by a backoff: 250 ms, doubling up to
	// `retryMax` (`-d veb_livereload_retry_max_ms`), reset once a stream opens.
	let retryDelay = retryFirst;
	let waitingTimer = 0;
	let waitingShown = false;
	const connect = () => {
		const events = new EventSource("/veb_livereload/" + started + "/events");
		events.addEventListener("open", () => {
			retryDelay = retryFirst;
			clearTimeout(waitingTimer);
			waitingTimer = 0;
			if (waitingShown) {
				waitingShown = false;
				ui.hideToast();
			}
		});
		events.addEventListener("error", () => {
			events.close();
			setTimeout(connect, retryDelay);
			retryDelay = Math.min(retryDelay * 2, retryMax);
			// Only mention the outage if it lasts; a restart usually takes milliseconds.
			if (!waitingTimer) {
				waitingTimer = setTimeout(() => {
					waitingShown = true;
					ui.toast("Waiting for the app", true);
				}, 1500);
			}
		});
		events.addEventListener("status", (e) => {
			const nl = e.data.indexOf("\n");
			const state = (nl < 0 ? e.data : e.data.slice(0, nl)).trim();
			const details = nl < 0 ? "" : e.data.slice(nl + 1);
			log("status:", state);
			if (state === "building") {
				ui.hideError();
				ui.toast("Rebuilding", true);
			} else if (state === "failed") {
				ui.hideToast();
				ui.showError(details.trim() || "The build failed. See the terminal for details.");
			} else {
				ui.hideToast();
			}
		});
		events.addEventListener("hello", (e) => {
			if (e.data !== serverId) {
				serverId = e.data;
				log("server restarted");
				refreshPage();
			}
		});
		events.addEventListener("change", (e) => {
			log("change:", e.data);
			if (e.data === "css") {
				refreshStylesheets();
			} else if (e.data === "full") {
				reload();
			} else {
				refreshPage();
			}
		});
	};
	connect();
})();
