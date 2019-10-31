// vlib/ui module header 

module ui

const ( 
	KEY_DELETE = 117 
	KEY_ESCAPE = 53 
	KEY_BACK = 51 
	KEY_ENTER = 36 
	KEY_SHIFT = 56 
	KEY_DOWN = 125 
	KEY_UP = 126 
	KEY_SUPER = 1 
)

const ( 
	DEFAULT_FONT_SIZE = 12 
)

const ( 
	ALIGN_RIGHT = 4 
	ALIGN_LEFT = 1 
)

struct Widget { 
	mut : 
	w int 
	h int 
	x int 
	y int 
	parent * Window 
	object voidptr 
	desc string 
	
}

type onclickfn fn ( wnd voidptr ) 


type onscrollfn fn ( wnd voidptr , dx , dy f64 ) 


type onmovefn fn ( wnd voidptr , x , y int ) 


type oncharfn fn ( wnd voidptr , code u32 , mods int ) 


type onkeydownfn fn ( wnd voidptr , c byte , mods , code int ) 


type onkeydown2fn fn ( wnd voidptr , key int , code int , action , mods int ) 


struct Window { 
	mut : 
	widget Widget 
	title string 
	mx int 
	my int 
	ns * NS 
	ptr voidptr 
	col_mode_x int 
	col_mode_y int 
	onclick_cb onclickfn 
	onscroll_cb onscrollfn 
	onmove_cb onmovefn 
	onchar_cb oncharfn 
	onkeydown_cb onkeydown2fn 
	
}

type drawfn fn ( ctx voidptr ) 


struct WinCfg { 
	width int 
	on_resize voidptr 
	ptr voidptr 
	height int 
	title string 
	draw_fn drawfn 
	borderless bool 
	resizable int 
	is_opengl int 
	is_modal int 
	is_browser bool 
	always_on_top int 
	url string 
	parent_wnd * Window 
	no_quit_menu bool 
	retina bool 
	
}

struct KeyEvent { 
	charr string 
	mods int 
	
}

struct CheckBox { 
	widget Widget 
	text string 
	
}

struct Size { 
	width int 
	height int 
	
}

type mydrawfn fn ( ) 


type onmousedownfn fn ( wnd voidptr , x , y int ) 


type rmbfn fn ( wnd voidptr , x , y int ) 


type onmouseupfn fn ( wnd voidptr , x , y int ) 


struct MyView { 
	uiwnd * Window 
	repo voidptr 
	mydraw mydrawfn 
	onclick_cb onclickfn 
	onmove_cb onmovefn 
	onmousedown_cb onmousedownfn 
	rmb_cb rmbfn 
	onmouseup_cb onmouseupfn 
	onscroll_cb onscrollfn 
	onkeydown_cb onkeydownfn 
	
}

struct NS { 
	w voidptr 
	view * MyView 
	nsgl voidptr 
	
}

struct Button { 
	pub : 
	widget Widget 
	text string 
	
}

struct ImageButton { 
	widget Widget 
	image int 
	
}

type ButtonClickFn fn ( btn * Button , ptr voidptr ) 


struct Menu { 
	cobj voidptr 
	parent_window * Window 
	
}

struct TextBox { 
	pub : 
	widget Widget 
	mut : 
	max_len int 
	is_multi bool 
	draw_border bool 
	placeholder string 
	
}

struct C . NSRect { 
}

struct C . NSColor { 
}



fn new_window (config WinCfg) * Window 
fn new_checkbox (text string, checked, enabled bool, parent mut Window) CheckBox 
fn (w mut Window) swap_buffers () 
fn (w mut Window) make_context_current () 
fn (wnd mut Window) refresh_rect (x, y, w, h int) 
fn (w mut Window) refresh () 
fn get_clipboard_text () string 
fn set_clipboard_text (s string) 
fn (w mut Window) set_draw_fn (mydraw voidptr) 
fn (w mut Window) set_title (title string) 
fn (w mut Window) set_size (width, height int) 
fn (widget mut Widget) set_width (width int) 
fn (widget mut Widget) set_pos (x, y int) 
fn (widget mut Widget) set_height (height int) 
fn (widget mut Widget) set_x (x int) 
fn (widget mut Widget) update_pos () 
fn (widget mut Widget) set_size (w, h int) 
fn init_v_ui () 
fn wait_events () 
fn post_empty_event () 
fn delete_all_cookies () 
fn open_url (u string) 

fn (w mut Window) onclick (cb voidptr) 
fn (w mut Window) onmousedown (cb voidptr) 
fn (w mut Window) onrmb (cb voidptr) 
fn (w mut Window) onmouseup (cb voidptr) 
fn (w mut Window) onkeydown (cb voidptr) 
fn (w mut Window) onmove (cb voidptr) 
fn (w mut Window) onscroll (cb voidptr) 
fn file_type_icon (typ string) gx . Image 
fn file_icon (path string) gx . Image 
fn notify (title, s string) 



fn (t TextBox) text () string 
fn (t mut TextBox) set_placeholder (val string) 
fn (t mut TextBox) focus () 
fn (t mut TextBox) unfocus () 
fn new_button (text string, parent * Window, onclick ButtonClickFn) Button 
fn draw_text (x, y int, text string, cfg gx . TextCfg) 
fn draw_text_def (x, y int, s string) 
fn draw_image (x, y, w, h int, img gx . Image) 
fn new_textbox (parent * Window, is_multi bool) TextBox 

fn reg_key_vid () 

fn draw_empty_rect (x, y, w, h int, color gx . Color) 
fn draw_rect (x, y, w, h int, color gx . Color) 
fn draw_circle (x, y, d int, color gx . Color) 
fn draw_line (x, y, x2, y2 int, color gx . Color) 
fn create_image (file string) gx . Image 
fn text_width (text string) int 

