// Currently there is only X11 Selections support and no way to handle Wayland
// but since Wayland isn't extremely adopted, we are covering almost all Linux distros.
module clipboard

import (
	time
	sync
	math
)

#flag -lX11
#include <X11/Xlib.h>

// X11
struct C.Display
struct C.Atom
struct C.Window
fn C.XInitThreads() int
fn C.XCloseDisplay(d &Display)
fn C.XFlush(d &Display)
fn C.XDestroyWindow(d &Display, w Window)
fn C.XNextEvent(d Display, e &XEvent)
fn C.XSetSelectionOwner(d &Display, a Atom, w Window, time int)
fn C.XGetSelectionOwner(d &Display, a Atom) Window
fn C.XChangeProperty(d &Display, requestor Window, property Atom, typ Atom, format int, mode int, data voidptr, nelements int) int
fn C.XSendEvent(d &Display, requestor Window, propogate int, mask i64, event &XEvent)
fn C.XInternAtom(d &Display, typ byteptr, only_if_exists int) Atom
fn C.XCreateSimpleWindow(d &Display, root Window, x int, y int, width u32, height u32, border_width u32, border u64, background u64) Window
fn C.XOpenDisplay(name byteptr) &Display
fn C.XConvertSelection(d &Display, selection Atom, target Atom, property Atom, requestor Window, time int) int
fn C.XSync(d &Display, discard int) int
fn C.XGetWindowProperty(d &Display, w Window, property Atom, offset i64, length i64, delete int, req_type Atom, actual_type_return &Atom, actual_format_return &int, nitems &i64, bytes_after_return &i64, prop_return &byteptr) int
fn C.XDeleteProperty(d &Display, w Window, property Atom) int
fn C.DefaultScreen() int
fn C.RootWindow() voidptr
fn C.BlackPixel() voidptr
fn C.WhitePixel() voidptr
fn C.XFree()

struct C.XSelectionRequestEvent{
	mut:
	selection Atom
	display &Display	/* Display the event was read from */
	owner Window
	requestor Window
	target Atom
	property Atom
	time int
}
struct C.XSelectionEvent{
	mut:
	@type int
	selection Atom
	display &Display	/* Display the event was read from */
	requestor Window
	target Atom
	property Atom
	time int
}
struct C.XSelectionClearEvent{
	mut:
	window Window
	selection Atom
}
struct C.XDestroyWindowEvent {
	mut:
	window Window
}
struct C.XEvent{
	mut:
	@type int
	xselectionrequest XSelectionRequestEvent
	xselection XSelectionEvent
	xselectionclear XSelectionClearEvent
	xdestroywindow XDestroyWindowEvent
}

const (
	atom_names = ["TARGETS", "CLIPBOARD", "PRIMARY", "SECONDARY", "TEXT", "UTF8_STRING", "text/plain", "text/html"]
)
//UNSUPPORTED TYPES: MULTIPLE, INCR, TIMESTAMP, image/bmp, image/jpeg, image/tiff, image/png

// all the atom types we need
// currently we only support text
// in the future, maybe we can extend this
// to support other mime types
enum atom_type {
	xa_atom = 0, //value 4
	xa_string = 1, //value 31
	targets = 2,
	clipboard = 3,
	primary = 4,
	secondary = 5,
	text = 6,
	utf8_string = 7
	text_plain = 8,
	text_html = 9
}

pub struct Clipboard {
	display &Display
	mut:
	selection Atom //the selection atom
	window Window
	atoms []Atom
	mutex &sync.Mutex
	text string // text data sent or received
	got_text bool // used to confirm that we have got the text
	is_owner bool // to save selection owner state
}

struct Property{
	actual_type Atom
	actual_format int
	nitems int
	data byteptr
}

fn new_clipboard() &Clipboard {
	return new_x11_clipboard(.clipboard)
}

// Initialize a new clipboard of the given selection type.
// We can initialize multiple clipboard instances and use them separately
fn new_x11_clipboard(selection atom_type) &Clipboard {
	if !(selection in [.clipboard, .primary, .secondary]) {
		panic("Wrong atom_type. Must be one of .primary, .secondary or .clipboard.")
	}

	//init x11 thread support
	status := XInitThreads()
	if status == 0 {
		println("WARN: this system does not support threads; clipboard will cause the program to lock.")
	}

	display := new_display()

	if display == C.NULL {
		println("ERROR: No X Server running. Clipboard cannot be used.")
		return &Clipboard{ display: 0 mutex: sync.new_mutex() }
	}

	mut cb := &Clipboard{
		display: display
		window: create_xwindow(display)
		mutex: sync.new_mutex()
	}
	cb.intern_atoms()
	cb.selection = cb.get_atom(selection)
	// start the listener on another thread or
	// we will be locked and will have to hard exit
	go cb.start_listener()
	return cb
}

fn (cb &Clipboard) check_availability() bool {
	return cb.display != C.NULL
}

fn (cb mut Clipboard) free() {
	XDestroyWindow(cb.display, cb.window)
	cb.window = Window(C.None)
	//FIX ME: program hangs when closing display
	//XCloseDisplay(cb.display)
}

fn (cb mut Clipboard) clear(){
	cb.mutex.lock()
	XSetSelectionOwner(cb.display, cb.selection, Window(C.None), C.CurrentTime)
	XFlush(cb.display)
	cb.is_owner = false
	cb.text = ""
	cb.mutex.unlock()
}

fn (cb &Clipboard) has_ownership() bool {
	return cb.is_owner
}

fn (cb &Clipboard) take_ownership(){
	XSetSelectionOwner(cb.display, cb.selection, cb.window, C.CurrentTime)
	XFlush(cb.display)
}

fn (cb mut Clipboard) set_text(text string) bool {
	if cb.window == Window(C.None) {return false}
	cb.mutex.lock()
	cb.text = text
	cb.is_owner = true
	cb.take_ownership()
	XFlush(cb.display)
	cb.mutex.unlock()
	// sleep a little bit
	time.sleep(1)
	return cb.is_owner
}

fn (cb mut Clipboard) get_text() string {
	if cb.window == Window(C.None) {return ""}
	if cb.is_owner {
		return cb.text
	}
	cb.got_text = false

	//Request a list of possible conversions, if we're pasting.
	XConvertSelection(cb.display, cb.selection, cb.get_atom(.targets), cb.selection, cb.window, C.CurrentTime)

	//wait for the text to arrive
	mut retries := 5
	for {
		if cb.got_text || retries == 0 {break}
		time.usleep(50000)
		retries--
	}
	return cb.text
}

// this function is crucial to handling all the different data types
// if we ever support other mimetypes they should be handled here
fn (cb mut Clipboard) transmit_selection(xse &XSelectionEvent) bool {
	if xse.target == cb.get_atom(.targets) {
		targets := cb.get_supported_targets()
		XChangeProperty(xse.display, xse.requestor, xse.property, cb.get_atom(.xa_atom), 32, C.PropModeReplace, targets.data, targets.len)
	} else if cb.is_supported_target(xse.target) && cb.is_owner && cb.text != "" {
		cb.mutex.lock()
		XChangeProperty(xse.display, xse.requestor, xse.property, xse.target, 8, C.PropModeReplace, cb.text.str, cb.text.len)
		cb.mutex.unlock()
	} else {
		return false
	}
	return true
}

fn (cb mut Clipboard) start_listener(){
	event := XEvent{}
	mut sent_request := false
	mut to_be_requested := Atom(0)
	for {
		XNextEvent(cb.display, &event)
		if (event.@type == 0) {
			println("error")
           	continue
        }
		match event.@type {
			C.DestroyNotify {
				if event.xdestroywindow.window == cb.window {
					return // we are done
				}
			}
			C.SelectionClear {
				if event.xselectionclear.window == cb.window && event.xselectionclear.selection == cb.selection {
					cb.mutex.lock()
					cb.is_owner = false
					cb.text = ""
					cb.mutex.unlock()
				}
			}
			C.SelectionRequest {
				if event.xselectionrequest.selection == cb.selection {
					mut xsre := &XSelectionRequestEvent{ display: 0 }
					xsre = &event.xselectionrequest

					mut xse := XSelectionEvent{
						@type: C.SelectionNotify // 31
						display: xsre.display
						requestor: xsre.requestor
						selection: xsre.selection
						time: xsre.time
						target: xsre.target
						property: xsre.property
					}
					if !cb.transmit_selection(&xse) {
						xse.property = new_atom(C.None)
					}
					XSendEvent(cb.display, xse.requestor, 0, C.PropertyChangeMask, &xse)
					XFlush(cb.display)
				}
			}
			C.SelectionNotify {
				if event.xselection.selection == cb.selection && event.xselection.property != Atom(C.None) {
					if event.xselection.target == cb.get_atom(.targets) && !sent_request {
						sent_request = true
						prop := read_property(cb.display, cb.window, cb.selection)
						to_be_requested = cb.pick_target(prop)
						if to_be_requested != Atom(0) {
							XConvertSelection(cb.display, cb.selection, to_be_requested, cb.selection, cb.window, C.CurrentTime)
						}
					} else if event.xselection.target == to_be_requested {
						sent_request = false
						to_be_requested = Atom(0)
						cb.mutex.lock()
						prop := read_property(event.xselection.display, event.xselection.requestor, event.xselection.property)
						XDeleteProperty(event.xselection.display, event.xselection.requestor, event.xselection.property)
						if cb.is_supported_target(prop.actual_type) {
							cb.got_text = true
							cb.text = string(prop.data) //TODO: return byteptr to support other mimetypes
						}
						cb.mutex.unlock()
					}
				}
			}
			C.PropertyNotify {}
			else {}
		}
	}
}



// Helpers

// Initialize all the atoms we need
fn (cb mut Clipboard) intern_atoms(){
	cb.atoms << Atom(4) //XA_ATOM
	cb.atoms << Atom(31) //XA_STRING
	for i, name in atom_names{
		only_if_exists := if i == int(atom_type.utf8_string) {1} else {0}
		cb.atoms << XInternAtom(cb.display, name.str, only_if_exists)
		if i == int(atom_type.utf8_string) && cb.atoms[i] == Atom(C.None) {
			cb.atoms[i] = cb.get_atom(.xa_string)
		}
	}
}

fn read_property(d &Display, w Window, p Atom) Property {
	actual_type := Atom(0)
	actual_format := 0
	nitems := 0
	bytes_after := 0
	ret := byteptr(0)
	mut read_bytes := 1024
	for {
		if ret != 0 {
			C.XFree(ret)
		}
		XGetWindowProperty(d, w, p, 0, read_bytes, 0, C.AnyPropertyType, &actual_type, &actual_format, &nitems, &bytes_after, &ret)
		read_bytes *= 2
		if bytes_after == 0 {break}
	}
	return Property{actual_type, actual_format, nitems, ret}
}

// Finds the best target given a local copy of a property.
fn (cb &Clipboard) pick_target(prop Property) Atom {
	//The list of targets is a list of atoms, so it should have type XA_ATOM
	//but it may have the type TARGETS instead.
	if((prop.actual_type != cb.get_atom(.xa_atom) && prop.actual_type != cb.get_atom(.targets)) || prop.actual_format != 32)
	{
		//This would be really broken. Targets have to be an atom list
		//and applications should support this. Nevertheless, some
		//seem broken (MATLAB 7, for instance), so ask for STRING
		//next instead as the lowest common denominator
		return cb.get_atom(.xa_string)
	}
	else
	{
		atom_list := &Atom(prop.data)

		mut to_be_requested := Atom(0)

		//This is higher than the maximum priority.
		mut priority := math.max_i32

		for i in 0..prop.nitems {
			//See if this data type is allowed and of higher priority (closer to zero)
			//than the present one.

			if cb.is_supported_target(atom_list[i]) {
				index := cb.get_target_index(atom_list[i])
				if(priority > index && index >= 0)
				{
					priority = index
					to_be_requested = atom_list[i]
				}
			}
		}
		return to_be_requested
	}
}

fn (cb &Clipboard) get_atoms(types ...atom_type) []Atom {
	mut atoms := []Atom
	for typ in types {
		atoms << cb.atoms[typ]
	}
	return atoms
}

fn (cb &Clipboard) get_atom(typ atom_type) Atom {
	return cb.atoms[typ]
}

fn (cb &Clipboard) is_supported_target(target Atom) bool {
	return cb.get_target_index(target) >= 0
}

fn (cb &Clipboard) get_target_index(target Atom) int {
	for i, atom in cb.get_supported_targets() {
		if atom == target {return i}
	}
	return -1
}

fn (cb &Clipboard) get_supported_targets() []Atom {
	return cb.get_atoms(atom_type.utf8_string, .xa_string, .text, .text_plain, .text_html)
}

fn new_atom(value int) &Atom {
	mut atom := &Atom{}
	atom = value
	return atom
}

fn create_xwindow(display &Display) Window {
	N := C.DefaultScreen(display)
	return XCreateSimpleWindow(display, C.RootWindow(display, N), 0, 0, 1, 1,
		0, C.BlackPixel(display, N), C.WhitePixel(display, N))
}

fn new_display() &Display {
	return XOpenDisplay(C.NULL)
}

// create a new PRIMARY clipboard (only supported on Linux)
pub fn new_primary() &Clipboard {
	return new_x11_clipboard(.primary)
}
