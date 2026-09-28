interface Layout {
mut:
	id int
}

interface Widget {
mut:
	id            int
	symbol        rune
	bytecode      char
	signed_size   isize
	unsigned_size usize
	tone          WidgetTone
}

enum WidgetTone {
	ready
}

struct Item {
mut:
	id            int
	symbol        rune
	bytecode      char
	signed_size   isize
	unsigned_size usize
	tone          WidgetTone
}

fn (w &Widget) read_id() int { return w.id }

fn (w &Widget) read_symbol() rune { return w.symbol }

fn (w &Widget) read_bytecode() char { return w.bytecode }

fn (w &Widget) read_signed_size() isize { return w.signed_size }

fn (w &Widget) read_unsigned_size() usize { return w.unsigned_size }

fn (w &Widget) read_tone() WidgetTone { return w.tone }

fn read(l Layout) int {
	p := l
	if p is Item { return Widget(p).read_id() }
	return 0
}

fn read_parenthesized(l Layout) int {
	p := l
	if p is Item { return (Widget(p)).read_id() }
	return 0
}

fn read_other_scalars(l Layout) bool {
	p := l
	if p is Item {
		return Widget(p).read_symbol() == `A`
			&& Widget(p).read_bytecode() == char(66)
			&& Widget(p).read_signed_size() == -3
			&& Widget(p).read_unsigned_size() == 9
			&& Widget(p).read_tone() == .ready
	}
	return false
}

fn test_readonly_interface_cast() {
	assert read(Item{ id: 42 }) == 42
	assert read_parenthesized(Item{ id: 42 }) == 42
	assert read_other_scalars(Item{
		id:            42
		symbol:        `A`
		bytecode:      char(66)
		signed_size:   -3
		unsigned_size: 9
		tone:          .ready
	})
}
