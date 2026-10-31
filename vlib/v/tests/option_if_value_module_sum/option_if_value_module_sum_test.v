// An `if`/`match` whose branches are options of a module-qualified type
// (`?mod.T(x)`, `?&mod.T(x)`, `none`) is an option value, as in V 0.5.2, even
// when some module declares a sum type with the same short name as `T`.
import ifoptdom
import ifoptstyle

struct Video {
mut:
	frame ?&ifoptdom.Frame
}

fn (mut video Video) set_current_video_frame(frame ?&ifoptdom.Frame) {
	video.frame = frame
}

fn (mut video Video) set_current_frame(width ?int) {
	video.set_current_video_frame(if w := width {
		?&ifoptdom.Frame(ifoptdom.Frame.new(w))
	} else {
		?&ifoptdom.Frame(none)
	})
}

@[unsafe]
fn initiator_scheme(document &ifoptdom.Document) string {
	initiator_origin := if document != unsafe { nil } {
		?ifoptdom.Origin(document.origin())
	} else {
		none
	}
	origin := initiator_origin or { return 'none' }
	return origin.scheme
}

fn test_if_option_value_of_a_module_sum_type() {
	context := ifoptstyle.Context{
		factor: 3
	}
	stop := ifoptstyle.ColorStop{
		hint: ifoptstyle.StyleValue(ifoptstyle.Length{2})
	}.absolutized(context)
	assert ifoptstyle.describe(stop.hint) == 'length 6'
	assert ifoptstyle.describe(stop.position) == 'none'
	keyword := ifoptstyle.ColorStop{
		position: ifoptstyle.StyleValue(ifoptstyle.Keyword{'auto'})
	}.absolutized(context)
	assert ifoptstyle.describe(keyword.hint) == 'none'
	assert ifoptstyle.describe(keyword.position) == 'keyword AUTO'
}

fn test_if_option_pointer_named_like_a_sum_type() {
	values := {
		1: &ifoptdom.StyleValue{
			n: 5
		}
	}
	assert ifoptstyle.lookup(values, 1) == 5
	assert ifoptstyle.lookup(values, 2) == -1
}

@[unsafe]
fn test_if_option_struct_named_like_another_modules_sum_type() {
	document := &ifoptdom.Document{
		origin_: ifoptdom.Origin{'https'}
	}
	assert initiator_scheme(document) == 'https'
	assert initiator_scheme(unsafe { nil }) == 'none'
}

fn test_if_option_pointer_argument() {
	mut video := Video{}
	video.set_current_frame(640)
	frame := video.frame or { panic('missing frame') }
	assert frame.width == 640
	video.set_current_frame(none)
	assert video.frame == none
}

fn test_if_option_cast_to_a_sum_type() {
	assert ifoptstyle.method_name(ifoptstyle.MethodStyleValue{30}) == 'polar 30'
	assert ifoptstyle.method_name(none) == 'rectangular oklab'
}

fn test_none_argument_and_none_branch_of_an_option_sum_field() {
	kept := ifoptstyle.shadow(true, 4)
	assert ifoptstyle.describe(kept.radius) == 'length 4'
	assert ifoptstyle.describe(kept.color) == 'keyword red'
	dropped := ifoptstyle.shadow(false, 4)
	assert ifoptstyle.describe(dropped.radius) == 'none'
}

fn test_match_option_value_of_a_module_sum_type() {
	assert ifoptstyle.for_init_names() == ['declaration 3', 'expression i']
	assert ifoptstyle.describe(ifoptstyle.shadow_from_match(0).radius) == 'none'
	assert ifoptstyle.describe(ifoptstyle.shadow_from_match(1).radius) == 'keyword thin'
	assert ifoptstyle.describe(ifoptstyle.shadow_from_match(2).radius) == 'length 2'
}
