module jsdom

import jsdom.ctx

pub struct HTMLCanvasElement {
	HTMLElement
}

pub fn (cv HTMLCanvasElement) height() int {
	ret := 0
	#ret.val = cv.node.height;

	return ret
}

pub fn (cv HTMLCanvasElement) width() int {
	ret := 0
	#ret.val = cv.node.width;

	return ret
}

pub fn (cv HTMLCanvasElement) typ() NodeType {
	return .element
}

pub fn (elem HTMLCanvasElement) add_event_listener(event string, cb EventCallback) {
	#elem.node.addEventListener(event.str, function (event) { let e = jsdom__dispatch_event_target(this);
	#let ev = jsdom__dispatch_event(event); ev.event = event;
	#return cb(e,ev)
	#});
}

pub fn (elem HTMLCanvasElement) get_context(ctx_ string) ctx.ContextResult {
	mut res := ctx.NoneContext{}
	#let ctx = elem.node.getContext(ctx_.str);
	#if (ctx instanceof CanvasRenderingContext2D) { res = new jsdom__ctx__CanvasRenderingContext2D(ctx); res.ctx = ctx; }

	return res
}
