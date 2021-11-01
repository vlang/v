module jsdom

pub interface IEvent {
	composed_path() []IEventTarget
}

pub struct JS.Event {}

pub struct JS.MouseEvent {}

pub struct Event {
	event JS.Event [noinit]
}

pub fn (ev Event) composed_path() []IEventTarget {
	mut composed := []IEventTarget{}
	#ev.event.composedPath().forEach((item) => {
	#array_push(composed, jsdom__dispatch_event())
	#})

	return composed
}

pub struct UIEvent {
	Event
}

// detail returns `int` with detail about the event, depending on the event type.
pub fn (ev UIEvent) detail() int {
	ret := 0
	#ret.val = ev.event.detail

	return ret
}

pub struct MouseEvent {
	UIEvent
}

// button returns the button number that was pressed (if applicable) when the mouse event was fired.
pub fn (ev MouseEvent) button() int {
	ret := 0
	#ret.val = ev.event.button;

	return ret
}

// alt_key returns `true` if the `alt` key was down when the mouse event was fired.
pub fn (ev MouseEvent) alt_key() bool {
	ret := false
	#ret.val = ev.event.altKey;

	return ret
}

// buttons returns the buttons being depressed (if any) when the mouse event was fired.
pub fn (ev MouseEvent) buttons() int {
	ret := 0
	#ret.val = ev.event.buttons;

	return ret
}

// client_x returns the X coordinate of the mouse pointer in local coordinates.
pub fn (ev MouseEvent) client_x() int {
	ret := 0
	#ret.val = ev.event.clientX;

	return ret
}

// client_y returns the Y coordinate of the mouse pointer in local coordinates.
pub fn (ev MouseEvent) client_y() int {
	ret := 0
	#ret.val = ev.event.clientY;

	return ret
}

// ctrl_key returns `true` if the `ctrl` key was down when the mouse event was fired.
pub fn (ev MouseEvent) ctrl_key() bool {
	ret := false
	#ret.val = ev.event.ctrlKey;

	return ret
}

// meta_key returns `true` if the `meta` key was down when the mouse event was fired.
pub fn (ev MouseEvent) meta_key() bool {
	ret := false
	#ret.val = ev.event.metaKey;

	return ret
}

// movenet_x reaturns the X coordinate of the mouse pointer relative to the position of the last `mousemove` event.
pub fn (ev MouseEvent) movement_x() int {
	ret := 0
	#ret.val = ev.event.movementX;

	return ret
}

// movenet_y reaturns the Y coordinate of the mouse pointer relative to the position of the last `mousemove` event.
pub fn (ev MouseEvent) movement_y() int {
	ret := 0
	#ret.val = ev.event.movementY;

	return ret
}

// offset_x returns the X coordinate of the mouse pointer relative to the position of the padding edge of the target node.
pub fn (ev MouseEvent) offset_x() int {
	ret := 0
	#ret.val = ev.event.offsetX;

	return ret
}

// offset_y reaturns the Y coordinate of the mouse pointer relative to the position of the padding edge of the target node.
pub fn (ev MouseEvent) offset_y() int {
	ret := 0
	#ret.val = ev.event.offsetY;

	return ret
}

pub fn (ev MouseEvent) composed_path() []IEventTarget {
	mut composed := []IEventTarget{}
	#ev.event.composedPath().forEach((item) => {
	#array_push(composed, jsdom__dispatch_event())
	#})

	return composed
}

pub struct AbortSignal {
	Event
}

pub fn (ev AbortSignal) composed_path() []IEventTarget {
	mut composed := []IEventTarget{}
	#ev.event.composedPath().forEach((item) => {
	#array_push(composed, jsdom__dispatch_event(item))
	#})

	return composed
}

pub fn (sig AbortSignal) aborted() bool {
	res := false
	#res.val = sig.event.aborted;

	return res
}

fn dispatch_event(event JS.Event) IEvent {
	mut ret := IEvent(Event{})
	#if (event instanceof AbortSignal) { ret = new jsdom__AbortSignal({});  }
	#else if (event instanceof MouseEvent) { ret = new jsdom__MouseEvent({}); }
	#ret.event = event

	return ret
}
