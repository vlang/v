module main

struct Layout {}

struct Window {}

interface View {
mut:
	content []View
	generate_layout(mut window Window) Layout
}

@[params]
struct TextCfg {
	text string
}

struct TextView {
	TextCfg
mut:
	content []View
}

fn (mut tv TextView) generate_layout(mut window Window) Layout {
	return Layout{}
}

fn text(cfg TextCfg) View {
	return TextView{
		text: cfg.text
	}
}

@[params]
struct ContainerCfg {
	content []View
}

struct ContainerView {
	ContainerCfg
mut:
	content []View
}

fn (mut cv ContainerView) generate_layout(mut window Window) Layout {
	return Layout{}
}

fn column(cfg ContainerCfg) View {
	return ContainerView{
		content: cfg.content
	}
}

fn row(cfg ContainerCfg) View {
	return ContainerView{
		content: cfg.content
	}
}

fn page(content []View) View {
	return column(
		content: content
	)
}

struct App {
pub mut:
	shares f32 = 5
}

fn build_page(app App, secrets []string) View {
	return page([
		row(
			content: [
				column(
					content: []View{len: int(app.shares), init: View(text(text: secrets[index]))}
				),
			]
		),
	])
}

fn flatten_texts(view View) []string {
	return match view {
		TextView {
			[view.text]
		}
		ContainerView {
			mut texts := []string{}
			for child in view.content {
				texts << flatten_texts(child)
			}
			texts
		}
		else {
			[]string{}
		}
	}
}

fn test_interface_array_init_inside_nested_short_syntax() {
	secrets := ['alpha', 'beta', 'gamma', 'delta', 'epsilon']
	view := build_page(App{}, secrets)
	assert flatten_texts(view) == secrets
}
