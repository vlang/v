module ui

pub union C.Event {
	KeyEvent              C.KEY_EVENT_RECORD
	MouseEvent            C.MOUSE_EVENT_RECORD
	WindowBufferSizeEvent C.WINDOW_BUFFER_SIZE_RECORD
	MenuEvent             C.MENU_EVENT_RECORD
	FocusEvent            C.FOCUS_EVENT_RECORD
}

[typedef]
pub struct C.INPUT_RECORD {
	EventType u16
	Event     C.Event
}

pub union C.uChar {
mut:
	UnicodeChar rune
	AsciiChar   u8
}

[typedef]
pub struct C.KEY_EVENT_RECORD {
	bKeyDown          int
	wRepeatCount      u16
	wVirtualKeyCode   u16
	wVirtualScanCode  u16
	uChar             C.uChar
	dwControlKeyState u32
}

[typedef]
pub struct C.MOUSE_EVENT_RECORD {
	dwMousePosition   C.COORD
	dwButtonState     u32
	dwControlKeyState u32
	dwEventFlags      u32
}

[typedef]
pub struct C.WINDOW_BUFFER_SIZE_RECORD {
	dwSize C.COORD
}

[typedef]
pub struct C.MENU_EVENT_RECORD {
	dwCommandId u32
}

[typedef]
pub struct C.FOCUS_EVENT_RECORD {
	bSetFocus int
}

[typedef]
pub struct C.COORD {
mut:
	X i16
	Y i16
}

[typedef]
pub struct C.SMALL_RECT {
mut:
	Left   u16
	Top    u16
	Right  u16
	Bottom u16
}

[typedef]
pub struct C.CONSOLE_SCREEN_BUFFER_INFO {
mut:
	dwSize              C.COORD
	dwCursorPosition    C.COORD
	wAttributes         u16
	srWindow            C.SMALL_RECT
	dwMaximumWindowSize C.COORD
}

fn C.ReadConsoleInput(hConsoleInput C.HANDLE, lpBuffer &C.INPUT_RECORD, nLength u32, lpNumberOfEventsRead &u32) bool

fn C.GetNumberOfConsoleInputEvents(hConsoleInput C.HANDLE, lpcNumberOfEvents &u32) bool

fn C.GetConsoleScreenBufferInfo(handle C.HANDLE, info &C.CONSOLE_SCREEN_BUFFER_INFO) bool
