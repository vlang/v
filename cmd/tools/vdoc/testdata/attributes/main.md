# module main


## Contents
- [draw](#draw)
- [Buttons.from](#Buttons.from)
- [Buttons.zero](#Buttons.zero)
- [Buttons](#Buttons)
  - [all](#all)
  - [has](#has)
  - [is_empty](#is_empty)
  - [press](#press)
- [Options](#Options)

## draw
```v
@[deprecated: 'use pub fn paint instead of <b>draw</b> & co']
@[deprecated_after: '2999-01-01']
fn draw(opts Options)
```

draw is shown with the attributes above its signature.

[[Return to contents]](#Contents)

## Buttons.from
```v
fn Buttons.from() Buttons
```

[[Return to contents]](#Contents)

## Buttons.zero
```v
fn Buttons.zero() Buttons
```

[[Return to contents]](#Contents)

## Buttons
```v
@[flag]
enum Buttons {
	left
	right
}
```

Buttons are flags for the mouse buttons.

[[Return to contents]](#Contents)

## all
## has
## is_empty
## press
```v
@[inline]
fn (b Buttons) press()
```

press is a method with an attribute.

[[Return to contents]](#Contents)

## Options
```v
@[params]
struct Options {
pub:
	width int
}
```

Options are the options for `draw`.

[[Return to contents]](#Contents)

#### Powered by vdoc.
