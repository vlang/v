import os

const vml_codegen_vexe = @VEXE
const vml_codegen_tests_dir = os.dir(@FILE)
const vml_codegen_v3_dir = os.dir(vml_codegen_tests_dir)
const vml_codegen_vlib_dir = os.dir(vml_codegen_v3_dir)
const vml_codegen_v3_src = os.join_path(vml_codegen_v3_dir, 'v3.v')

fn vml_codegen_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_vml_codegen_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${vml_codegen_vexe} -gc none -path "${vml_codegen_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${vml_codegen_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn vml_codegen_mock_ui2() string {
	return (os.read_file(@FILE) or { panic(err) }).all_after('/* MOCK_UI2\n').all_before('\nMOCK_UI2 */')
}

fn test_vml_lowers_to_direct_ui2_elements() {
	v3_bin := vml_codegen_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_vml_codegen_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'ui2', 'core')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'vml_codegen' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'ui2', 'v.mod'), "Module { name: 'ui2', subdirs: ['core'] }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'ui2', 'core', 'ui2.v'), vml_codegen_mock_ui2()) or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'form.vml'), 'Screen {
    id: root
    width: root.half
    property f64 half: root.width / 2
    Repeater {
        model: app.items
        key: item.id
        Label {
            text: "\${item.name}:\${index}"
            width: root.half
            on_tap: app.select(item.id)
        }
    }
	ProgressBar { value: 25 max: 50 }
	Slider {
		id: volume
		bind.value: app.level
		min: 0
		max: 100
		step: 0.5
		orientation: vertical
		value_track: true
		on_change: app.select(4)
	}
	Switch { id: notifications bind.active: app.enabled on_active: app.select(5) }
	Spinner {
		id: location
		bind.text: app.location
		text_autoupdate: true
		on_text: app.choose("Work")
		Option { text: "Home" }
		Option { text: "Work" }
	}
	Checkbox { text: "Ready" checked: true }
	Rectangle { border_width: 2 border_right: 4 }
	Button { text: "Select" on_tap: app.select(app.selected) }
}') or { panic(err) }
	source := "module main

import ui2

struct Item {
	id int
	name string
}

struct App {
	items []Item
pub mut:
	selected int
	level    f64
	enabled  bool
	location string
}

pub fn (mut app App) select(id int) {
	app.selected = id
}

pub fn (mut app App) choose(value string) {
	app.location = value
}

fn (mut app App) private_action() {
	app.selected = -1
}

fn build(app &App) ui2.Element {
	return \$vml('form.vml')
}

fn main() {
	app := App{items: [Item{id: 7, name: 'a'}, Item{id: 8, name: 'b'}], level: 12.5, location: 'Home'}
	root := build(&app)
	mut mutable_app := app
	ui2.dispatch[App](mut mutable_app, 'select', 9) or { panic(err) }
	println(root.children.len.str() + ' ' + root.children[0].text + ' ' + root.children[0].key + ' ' + root.children[0].action_id)
	println(mutable_app.selected.str() + ' ' + root.children[2].accessibility_role)
	println(root.children[3].accessibility_role + ' ' + root.children[3].value.str() + ':' + root.children[3].min_value.str() + ':' + root.children[3].max_value.str() + ' ' + root.children[3].action_id)
	println(root.children[4].accessibility_role + ' ' + root.children[4].checked.str() + ' ' + root.children[4].action_id)
	println(root.children[5].accessibility_role + ' ' + root.children[5].text + ' ' + root.children[5].action_id)
	println(root.children[6].accessibility_role + ' ' + root.children[7].box.border_left.str() + ':' + root.children[7].box.border_right.str() + ' ' + root.children[8].action_id)
}
"
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, source) or { panic(err) }
	bin := os.join_path(root, 'vml_codegen')
	compile := os.execute('${v3_bin} -nocache -path "${root}|${vml_codegen_vlib_dir}" -b c -o ${bin} ${main_path}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '9 a:0 7 select:7\n9 progressbar\nslider 12.5:0.0:100.0 select:4\nswitch false select:5\ncombobox Home choose:Work\ncheckbox 2.0:4.0 select:app.selected', run.output
	os.write_file(os.join_path(root, 'review.vml'), 'Screen {
	id: review_root
	property f64 half: review_root.quarter * 2
	property f64 quarter: 25
	property int custom_selected: 1
	property color accent: "#00ff00"
	width: review_root.half
	background: "#ff0000"
	Rectangle { id: first width: 123 }
	Label { width: first.width }
	Repeater {
		model: app.items
		key: item
		Rectangle { id: repeated_first width: 77 }
		Button { width: repeated_first.width on_tap: app.select(index + 1) }
	}
	Label { text: 1 + 2 }
	Label { text: app.count + 1 }
	Label { text: "count:" + app.count }
	Label { text: app.display_name() }
	Button { on_tap: app.select(review_root.custom_selected) }
	Rectangle { width: app.content_width() background: review_root.accent }
	Label { text: 4 - 1 }
	Label { text: 3 * 4 }
	Label { text: 8 / 2 }
	Label { text: 7 % 4 }
	Button { on_tap: app.select(-1) }
	Label { text: app.ready && app.enabled }
	Label { text: app.count > 0 }
}') or { panic(err) }
	review_path := os.join_path(root, 'review.v')
	os.write_file(review_path, "module main\n\nimport ui2\n\nstruct App {\n\titems []int\n\tcount int\n\tready bool\n\tenabled bool\npub mut:\n\tselected int\n}\n\npub fn (mut app App) select(value int) {\n\tapp.selected = value\n}\n\npub fn (app &App) display_name() string {\n\treturn 'display:\${app.count}'\n}\n\npub fn (app &App) content_width() int {\n\treturn 88\n}\n\nfn build(app &App) ui2.Element {\n\treturn \$vml('review.vml')\n}\n\nfn main() {\n\tapp := App{items: [10], count: 2, ready: true}\n\troot := build(&app)\n\tprintln(root.children[4].frame.width.str() + ' ' + root.box.bg.str() + ' ' + root.children[1].frame.width.str() + ' ' + root.children[3].frame.width.str() + ' ' + root.children[3].action_id + ' ' + root.children[4].text + ' ' + root.children[5].text + ' ' + root.children[6].text + ' ' + root.children[7].text + ' ' + root.children[8].action_id + ' ' + root.children[9].frame.width.str() + ':' + root.children[9].box.bg.str() + ' ' + root.children[10].text + ' ' + root.children[11].text + ' ' + root.children[12].text + ' ' + root.children[13].text + ' ' + root.children[14].action_id + ' ' + root.children[15].text + ' ' + root.children[16].text)\n}\n") or {
		panic(err)
	}
	review_compile := os.execute('${v3_bin} -nocache -path "${root}|${vml_codegen_vlib_dir}" -b c -o ${bin} ${review_path}')
	assert review_compile.exit_code == 0, review_compile.output
	review_run := os.execute(bin)
	assert review_run.exit_code == 0, review_run.output
	assert review_run.output.trim_space() == '50.0 16711680 123.0 77.0 select:1 3 3 count:2 display:2 select:1 88.0:65280 3 12 4 3 select:-1 false true', review_run.output
	os.write_file(os.join_path(root, 'events.vml'), 'Screen {
	TextField { id: message on_text: app.choose("Typed") }
	TextArea { id: notes on_text: app.choose("Notes") }
}') or { panic(err) }
	events_path := os.join_path(root, 'events.v')
	os.write_file(events_path, "module main\n\nimport ui2\n\nstruct App {}\n\npub fn (mut app App) choose(value string) {\n\t_ = app\n\t_ = value\n}\n\nfn build(app &App) ui2.Element {\n\treturn \$vml('events.vml')\n}\n\nfn main() {\n\tapp := App{}\n\troot := build(&app)\n\tprintln(root.children[0].action_id + ':' + root.children[0].emit_change.str() + ' ' + root.children[1].action_id + ':' + root.children[1].emit_change.str())\n}\n") or {
		panic(err)
	}
	events_compile := os.execute('${v3_bin} -nocache -path "${root}|${vml_codegen_vlib_dir}" -b c -o ${bin} ${events_path}')
	assert events_compile.exit_code == 0, events_compile.output
	events_run := os.execute(bin)
	assert events_run.exit_code == 0, events_run.output
	assert events_run.output.trim_space() == 'choose:Typed:true choose:Notes:true', events_run.output
	os.write_file(os.join_path(root, 'nested.vml'), 'Screen {
	Repeater {
		model: app.rows
		key: item.id
		Repeater {
			model: item.values
			key: item
			Label { text: item }
		}
	}
}') or { panic(err) }
	nested_path := os.join_path(root, 'nested.v')
	os.write_file(nested_path, "module main\n\nimport ui2\n\nstruct Row {\n\tid int\n\tvalues []int\n}\n\nstruct App {\n\trows []Row\n}\n\nfn build(app &App) ui2.Element {\n\treturn \$vml('nested.vml')\n}\n\nfn main() {\n\tapp := App{rows: [Row{id: 10, values: [1]}, Row{id: 20, values: [1]}]}\n\troot := build(&app)\n\tprintln(root.children[0].key + ' ' + root.children[1].key)\n}\n") or {
		panic(err)
	}
	nested_compile := os.execute('${v3_bin} -nocache -path "${root}|${vml_codegen_vlib_dir}" -b c -o ${bin} ${nested_path}')
	assert nested_compile.exit_code == 0, nested_compile.output
	nested_run := os.execute(bin)
	assert nested_run.exit_code == 0, nested_run.output
	assert nested_run.output.trim_space() == '10:1 20:1', nested_run.output
	os.write_file(os.join_path(root, 'geometry_color.vml'), 'Screen {
	Rectangle { id: box width: 100 height: box.width background: app.theme_color() }
	Rectangle { background: app.numeric_color() }
}') or { panic(err) }
	geometry_color_path := os.join_path(root, 'geometry_color.v')
	os.write_file(geometry_color_path, "module main\n\nimport ui2\n\nstruct App {}\n\npub fn (app &App) theme_color() string {\n\treturn '#112233'\n}\n\npub fn (app &App) numeric_color() u32 {\n\treturn u32(0x445566)\n}\n\nfn build(app &App) ui2.Element {\n\treturn \$vml('geometry_color.vml')\n}\n\nfn main() {\n\tapp := App{}\n\troot := build(&app)\n\tprintln(root.children[0].frame.width.str() + ':' + root.children[0].frame.height.str() + ':' + root.children[0].box.bg.str() + ' ' + root.children[1].box.bg.str())\n}\n") or {
		panic(err)
	}
	geometry_color_compile := os.execute('${v3_bin} -nocache -path "${root}|${vml_codegen_vlib_dir}" -b c -o ${bin} ${geometry_color_path}')
	assert geometry_color_compile.exit_code == 0, geometry_color_compile.output
	geometry_color_run := os.execute(bin)
	assert geometry_color_run.exit_code == 0, geometry_color_run.output
	assert geometry_color_run.output.trim_space() == '100.0:100.0:1122867 4478310', geometry_color_run.output
	os.write_file(os.join_path(root, 'custom.vml'), 'Screen {
	id: root
	property Item selected: app.item
	property f32 ratio: 1 / 2
	Label { text: root.selected.name }
	Rectangle { width: root.ratio }
}') or { panic(err) }
	custom_path := os.join_path(root, 'custom.v')
	os.write_file(custom_path, "module main\n\nimport ui2\n\nstruct Item {\n\tname string\n}\n\nstruct App {\n\titem Item\n}\n\nfn build(app &App) ui2.Element {\n\treturn \$vml('custom.vml')\n}\n\nfn main() {\n\tapp := App{item: Item{name: 'chosen'}}\n\troot := build(&app)\n\tprintln(root.children[0].text + ' ' + root.children[1].frame.width.str())\n}\n") or {
		panic(err)
	}
	custom_compile := os.execute('${v3_bin} -nocache -path "${root}|${vml_codegen_vlib_dir}" -b c -o ${bin} ${custom_path}')
	assert custom_compile.exit_code == 0, custom_compile.output
	custom_run := os.execute(bin)
	assert custom_run.exit_code == 0, custom_run.output
	assert custom_run.output.trim_space() == 'chosen 0.5', custom_run.output
	os.write_file(os.join_path(root, 'expressions.vml'), r'Screen {
	Repeater {
		model: app.values
		key: item
		Label { text: item-1 }
	}
	Repeater {
		model: app.items
		key: item.value
		Rectangle { background: item.color }
	}
	Label { text: "${app.label(\"}\")}" }
}') or { panic(err) }
	expressions_path := os.join_path(root, 'expressions.v')
	os.write_file(expressions_path, "module main\n\nimport ui2\n\nstruct Item {\n\tvalue int\n\tcolor string\n}\n\nstruct App {\n\tvalues []int\n\titems []Item\n}\n\nfn (app &App) label(value string) string {\n\t_ = app\n\treturn value\n}\n\nfn build(app &App) ui2.Element {\n\treturn \$vml('expressions.vml')\n}\n\nfn main() {\n\tapp := App{values: [10], items: [Item{value: 1, color: '#112233'}]}\n\troot := build(&app)\n\tprintln(root.children[0].text + ' ' + root.children[1].box.bg.str() + ' ' + root.children[2].text)\n}\n") or {
		panic(err)
	}
	expressions_compile := os.execute('${v3_bin} -nocache -path "${root}|${vml_codegen_vlib_dir}" -b c -o ${bin} ${expressions_path}')
	assert expressions_compile.exit_code == 0, expressions_compile.output
	expressions_run := os.execute(bin)
	assert expressions_run.exit_code == 0, expressions_run.output
	assert expressions_run.output.trim_space() == '9 1122867 }', expressions_run.output
	os.write_file(os.join_path(root, 'form.vml'), 'Screen { MessageBox { Button { on_tap: app.missing() } } }') or { panic(err) }
	invalid := os.execute('${v3_bin} -nocache -path "${root}|${vml_codegen_vlib_dir}" -b c -o ${bin} ${main_path}')
	assert invalid.exit_code != 0, invalid.output
	assert invalid.output.contains('missing'), invalid.output
	os.write_file(os.join_path(root, 'form.vml'), 'Screen { Button { MenuItem { on_tap: app.missing() } } }') or { panic(err) }
	invalid_menu := os.execute('${v3_bin} -nocache -path "${root}|${vml_codegen_vlib_dir}" -b c -o ${bin} ${main_path}')
	assert invalid_menu.exit_code != 0, invalid_menu.output
	assert invalid_menu.output.contains('missing'), invalid_menu.output
	os.write_file(os.join_path(root, 'form.vml'), 'Screen { Button { on_tap: app.private_action() } }') or { panic(err) }
	invalid_private := os.execute('${v3_bin} -nocache -path "${root}|${vml_codegen_vlib_dir}" -b c -o ${bin} ${main_path}')
	assert invalid_private.exit_code != 0, invalid_private.output
	assert invalid_private.output.contains('must be public'), invalid_private.output
}

/* MOCK_UI2
module ui2

pub enum Kind { screen view label image button checkbox dropdown text_field text_area scroll slider switch_control }
pub enum Align { left center right }
pub enum Orientation { horizontal vertical }
pub struct Rect { pub: x f64 y f64 width f64 height f64 }
pub struct BoxStyle { pub: bg u32 = 0xffffff radius f64 transparent bool border_color u32 border_left f64 border_top f64 border_right f64 border_bottom f64 }
pub struct TextStyle { pub: color u32 = 0x111111 background_color u32 size f64 = 15 font_family string bold bool italic bool underline bool strikethrough bool shadow bool outline bool vertical_align string link string align Align head_indent f64 first_line_indent f64 hyphenation_factor f64 lines int = 1 }
pub struct MenuEntry { pub: id string title string }
pub struct SliderStyle { pub: track_color u32 value_track_color u32 thumb_color u32 track_width f64 thumb_size f64 }
pub struct SwitchStyle { pub: inactive_track_color u32 active_track_color u32 thumb_color u32 disabled_track_color u32 disabled_thumb_color u32 }
pub struct Element { pub: kind Kind id string action_id string submit_id string key string text string checked bool image_path string tooltip string placeholder string frame Rect box BoxStyle text_style TextStyle native_style bool keyboard int emit_change bool long_press bool swipe_left bool readonly bool persistent_scrollbars bool secure bool clickable bool draggable bool rotation f64 cursor string menu []MenuEntry children []Element hidden bool enabled bool = true accessibility_role string accessibility_label string accessibility_value string autocorrect bool = true padding_left f64 = 12 value f64 min_value f64 max_value f64 step f64 orientation Orientation padding f64 value_track bool slider_style SliderStyle switch_style SwitchStyle }
pub const keyboard_default = 0
pub const keyboard_decimal = 8
pub fn bounds() Rect { return Rect{width: 800, height: 600} }
pub fn rect(x f64, y f64, width f64, height f64) Rect { return Rect{x, y, width, height} }
pub fn parse_hex_color(value string) u32 { return if value == '#112233' { u32(0x112233) } else { u32(0) } }
pub struct ProgressBarConfig { pub: id string frame Rect value f64 max f64 = 100 background u32 color u32 radius f64 }
pub fn progress_bar(config ProgressBarConfig) Element { return Element{kind: .view, id: config.id, frame: config.frame, accessibility_role: 'progressbar', accessibility_label: 'Progress', accessibility_value: '${config.value} of ${config.max}'} }
pub struct SliderConfig { pub: id string action_id string frame Rect min f64 max f64 = 100 value f64 step f64 orientation Orientation padding f64 value_track bool style SliderStyle }
pub fn slider(config SliderConfig) Element { return Element{kind: .slider, id: config.id, action_id: config.action_id, frame: config.frame, value: config.value, min_value: config.min, max_value: config.max, step: config.step, orientation: config.orientation, padding: config.padding, value_track: config.value_track, slider_style: config.style, accessibility_role: 'slider', accessibility_label: 'Slider', accessibility_value: config.value.str()} }
pub struct SwitchConfig { pub: id string action_id string frame Rect active bool style SwitchStyle }
pub fn switch_control(config SwitchConfig) Element { return Element{kind: .switch_control, id: config.id, action_id: config.action_id, frame: config.frame, checked: config.active, switch_style: config.style, accessibility_role: 'switch', accessibility_label: 'Switch', accessibility_value: if config.active { 'on' } else { 'off' }} }
pub struct SpinnerConfig { pub: id string action_id string frame Rect text string values []string text_autoupdate bool box BoxStyle text_style TextStyle }
pub fn spinner(config SpinnerConfig) Element { return Element{kind: .dropdown, id: config.id, action_id: config.action_id, frame: config.frame, text: if config.text_autoupdate && config.values.len > 0 { config.values[0] } else { config.text }, box: config.box, text_style: config.text_style, accessibility_role: 'combobox', accessibility_label: 'Spinner'} }
pub struct MessageBoxAction { pub: id string action_id string title string }
pub struct MessageBoxConfig { pub: id string frame Rect title string text string hidden bool width f64 height f64 actions []MessageBoxAction }
pub fn custom_message_box(config MessageBoxConfig) Element { return Element{kind: .view, id: config.id, frame: config.frame, text: config.text, hidden: config.hidden} }
pub fn compiled_vml_event(_ string, _ string, _ string, action string) string { return action }
pub fn compiled_vml_event_arg(_ string, _ string, _ string, action string, argument string) string { return action + ':' + argument }
pub fn compiled_vml_event_arg_path(_ string, _ string, _ string, action string, path string) string { return action + ':' + path }
pub fn dispatch[T](mut model T, name string, argument int) ! {
	$for method in T.methods {
		if method.name == name {
			$if method.is_pub && method.typ is fn ( int ) {
				model.$method(argument)
				return
			} $else {
				return error('unsupported method')
			}
		}
	}
	return error('unknown method')
}
MOCK_UI2 */
