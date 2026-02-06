// vtest build: false // requires special compilation flags: `-b wasm -os browser`
fn JS.change_color_by_id(ptr u8, len int, color_ptr u8, color_len int)

// `main` must be public!
pub fn main() {
	println('starting main.main...')
	change_color_by_id('description', 'red')
	change_color_by_id('description1', 'green')
	change_color_by_id('description2', 'blue')
	change_color_by_id('description3', 'black')
}

pub fn click_callback() {
	println('Hello from V')
}

fn change_color_by_id(id string, color string) {
	JS.change_color_by_id(id.str, id.len, color.str, color.len)
}
