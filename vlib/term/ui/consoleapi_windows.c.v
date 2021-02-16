module ui

import os

union C.Event {
	KeyEvent              C.KEY_EVENT_RECORD
	MouseEvent            C.MOUSE_EVENT_RECORD
	WindowBufferSizeEvent C.WINDOW_BUFFER_SIZE_RECORD
	MenuEvent             C.MENU_EVENT_RECORD
	FocusEvent            C.FOCUS_EVENT_RECORD
}

[typedef]
struct C.INPUT_RECORD {
	EventType u16
	Event     C.Event
}

union C.uChar {
	UnicodeChar rune
	AsciiChar   byte
}

[typedef]
struct C.KEY_EVENT_RECORD {
	bKeyDown          int
	wRepeatCount      u16
	wVirtualKeyCode   u16
	wVirtualScanCode  u16
	uChar             C.uChar
	dwControlKeyState u32
}

[typedef]
struct C.MOUSE_EVENT_RECORD {
	dwMousePosition   C.COORD
	dwButtonState     u32
	dwControlKeyState u32
	dwEventFlags      u32
}

[typedef]
struct C.WINDOW_BUFFER_SIZE_RECORD {
	dwSize C.COORD
}

[typedef]
struct C.MENU_EVENT_RECORD {
	dwCommandId u32
}

[typedef]
struct C.FOCUS_EVENT_RECORD {
	bSetFocus int
}

fn C.ReadConsoleInput() bool

fn C.GetNumberOfConsoleInputEvents() bool
