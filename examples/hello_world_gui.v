// Please share your thoughts, suggestions, questions, etc here:
// https://github.com/vlang-io/V/issues/3

// I'm very interested in your feedback.

module main

import ui  // Native cross platform ui toolkit (uses Cocoa, win32, GTK+)  

// There are no globals, so we have to use a context struct 
struct Context {
    input ui.TextBox // this uses native conrols (NSTextView on macOS, edit HWND on Windows)  
    names []string   // let's log the names to demonstrate how arrays work  
}

fn main() {
    wnd := ui.new_window(ui.WindowCfg{  // V has no default arguments and overloading. 
        width:  600                     // All stdlib functions with many args use Cfg wrappers.
        height: 300 
        title:  'hello world' 
    }) 
    ctx := Context{
        input: ui.new_textbox(wnd) 
        // we don't need to initialize the names array, it's done automatically  
    }
    ctx.input.set_placeholder('Enter your name')
    btn := ui.new_button(wnd, 'Click me', ctx.btn_click)
    for {
        ui.wait_events()
    }
}

fn (ctx mut Context) btn_click() { 
    name := ctx.input.text()
    ctx.input.hide()
    println('current list of names: $ctx.names')  // >> current list of names: [ "Bob", "Alex" ] 
    ui.alert('Hello, $name!')
    if ctx.names.contains(name) {
        ui.alert('I already greeted you ;)') 
    } 
    ctx.names << name 
}
