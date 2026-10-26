# module main


## Contents
- [draw](#draw)
- [Buttons.from](#Buttons.from)
- [Buttons.zero](#Buttons.zero)
- [Buttons](#Buttons)
  - [is_empty](#is_empty)
  - [has](#has)
  - [all](#all)
  - [set](#set)
  - [set_all](#set_all)
  - [clear](#clear)
  - [clear_all](#clear_all)
  - [toggle](#toggle)
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
fn Buttons.from[W](input W) !Buttons
```

from returns the `Buttons` value of an integer or of a field name, or an error if there is none.

[[Return to contents]](#Contents)

## Buttons.zero
```v
fn Buttons.zero() Buttons
```

zero returns a `Buttons` value with no flags set.

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

## is_empty
```v
fn (e &Buttons) is_empty() bool
```

is_empty returns true if no flags are set.

[[Return to contents]](#Contents)

## has
```v
fn (e &Buttons) has(flag Buttons) bool
```

has returns true if any of the flags in `flag` are set.

[[Return to contents]](#Contents)

## all
```v
fn (e &Buttons) all(flag Buttons) bool
```

all returns true if all the flags in `flag` are set.

[[Return to contents]](#Contents)

## set
```v
fn (mut e Buttons) set(flag Buttons)
```

set sets the flags in `flag`.

[[Return to contents]](#Contents)

## set_all
```v
fn (mut e Buttons) set_all()
```

set_all sets all the flags.

[[Return to contents]](#Contents)

## clear
```v
fn (mut e Buttons) clear(flag Buttons)
```

clear clears the flags in `flag`.

[[Return to contents]](#Contents)

## clear_all
```v
fn (mut e Buttons) clear_all()
```

clear_all clears all the flags.

[[Return to contents]](#Contents)

## toggle
```v
fn (mut e Buttons) toggle(flag Buttons)
```

toggle flips the flags in `flag`.

[[Return to contents]](#Contents)

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
