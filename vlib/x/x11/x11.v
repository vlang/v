// X11 type definitions - shared across all V modules
// Import this module instead of defining X11 types yourself
module x11

// Basic X11 types
pub type Window = u64
pub type Atom = u64
pub type VisualID = usize
pub type Time = u64
pub type Colormap = u64

// Forward declarations
pub struct C.Display {}

// X11 event types
@[typedef]
pub struct C.XSelectionRequestEvent {
pub mut:
	display   &C.Display = unsafe { nil }
	owner     Window
	requestor Window
	selection Atom
	target    Atom
	property  Atom
	time      int
}

@[typedef]
pub struct C.XSelectionEvent {
pub mut:
	type      int
	display   &C.Display = unsafe { nil }
	requestor Window
	selection Atom
	target    Atom
	property  Atom
	time      int
}

@[typedef]
pub struct C.XSelectionClearEvent {
pub mut:
	window    Window
	selection Atom
}

@[typedef]
pub struct C.XDestroyWindowEvent {
pub mut:
	window Window
}

@[typedef]
pub struct C.XPropertyEvent {
pub mut:
	state int
	atom  Atom
}

@[typedef]
pub struct C.XClientMessageEvent {
pub mut:
	window       Window
	format       int
	message_type Atom
	data         C.XClientMessageData
}

@[typedef]
pub union C.XClientMessageData {
pub mut:
	b [20]u8
	s [10]i16
	l [5]i64
}

@[typedef]
pub struct C.XGenericEventCookie {
pub mut:
	extension int
	evtype    int
	data      voidptr
}

@[typedef]
pub struct C.XKeyEvent {
pub mut:
	keycode u32
	state   u32
}

@[typedef]
pub struct C.XButtonEvent {
pub mut:
	button u32
	state  u32
	x      int
	y      int
}

@[typedef]
pub struct C.XMotionEvent {
pub mut:
	x     int
	y     int
	state u32
}

@[typedef]
pub struct C.XCrossingEvent {
pub mut:
	x     int
	y     int
	state u32
}

@[typedef]
pub struct C.XFocusChangeEvent {
pub mut:
	mode int
}

// Main XEvent union - each module should define this themselves based on their needs
// pub union C.XEvent { ... }

// X11 function declarations
pub fn C.XNextEvent(display &C.Display, event &C.XEvent) int
pub fn C.XCheckTypedWindowEvent(display &C.Display, window Window, event_type int, event &C.XEvent) int
pub fn C.XSendEvent(display &C.Display, window Window, propagate int, event_mask i64, event &C.XEvent) int
pub fn C.XFilterEvent(event &C.XEvent, window Window) int
pub fn C.XChangeProperty(display &C.Display, window Window, property Atom, type_ Atom, format int, mode int, data &u8, nitems int) int
pub fn C.XConvertSelection(display &C.Display, selection Atom, target Atom, property Atom, requestor Window, time Time) int
pub fn C.XSetSelectionOwner(display &C.Display, selection Atom, window Window, time Time) Window
pub fn C.XGetSelectionOwner(display &C.Display, selection Atom) Window
pub fn C.XDeleteProperty(display &C.Display, window Window, property Atom) int
pub fn C.XGetWindowProperty(display &C.Display, window Window, property Atom, offset i64, length i64, delete int, req_type Atom, actual_type &Atom, actual_format &int, nitems &u64, bytes_after &u64, data &&u8) int
