# module main


## Contents
- [Fill.from](#Fill.from)
- [Shade.from](#Shade.from)
- [Sides.from](#Sides.from)
- [Sides.zero](#Sides.zero)
- [Fill](#Fill)
- [Shade](#Shade)
- [Sides](#Sides)
  - [is_empty](#is_empty)
  - [has](#has)
  - [all](#all)
  - [set](#set)
  - [set_all](#set_all)
  - [clear](#clear)
  - [clear_all](#clear_all)
  - [toggle](#toggle)

## Fill.from
```v
fn Fill.from[W](input W) !Fill
```

from returns the `Fill` value of an integer or of a field name, or an error if there is none.

[[Return to contents]](#Contents)

## Shade.from
```v
fn Shade.from(name string) Shade
```

Shade.from returns the shade called `name`.

[[Return to contents]](#Contents)

## Sides.from
```v
fn Sides.from[W](input W) !Sides
```

from returns the `Sides` value of an integer or of a field name, or an error if there is none.

[[Return to contents]](#Contents)

## Sides.zero
```v
fn Sides.zero() Sides
```

zero returns a `Sides` value with no flags set.

[[Return to contents]](#Contents)

## Fill
```v
enum Fill {
	none
	solid
}
```

Fill is how a shape is filled.

[[Return to contents]](#Contents)

## Shade
```v
enum Shade {
	light
	dark
}
```

Shade has its own `from`, which replaces the generated one.

[[Return to contents]](#Contents)

## Sides
```v
@[flag]
enum Sides {
	top
	right
	bottom
	left
}
```

Sides are the sides of a shape that have a border.

[[Return to contents]](#Contents)

## is_empty
```v
fn (e &Sides) is_empty() bool
```

is_empty returns true if no flags are set.

[[Return to contents]](#Contents)

## has
```v
fn (e &Sides) has(flag Sides) bool
```

has returns true if any of the flags in `flag` are set.

[[Return to contents]](#Contents)

## all
```v
fn (e &Sides) all(flag Sides) bool
```

all returns true if all the flags in `flag` are set.

[[Return to contents]](#Contents)

## set
```v
fn (mut e Sides) set(flag Sides)
```

set sets the flags in `flag`.

[[Return to contents]](#Contents)

## set_all
```v
fn (mut e Sides) set_all()
```

set_all sets all the flags.

[[Return to contents]](#Contents)

## clear
```v
fn (mut e Sides) clear(flag Sides)
```

clear clears the flags in `flag`.

[[Return to contents]](#Contents)

## clear_all
```v
fn (mut e Sides) clear_all()
```

clear_all clears all the flags.

[[Return to contents]](#Contents)

## toggle
```v
fn (mut e Sides) toggle(flag Sides)
```

toggle flips the flags in `flag`.

[[Return to contents]](#Contents)

#### Powered by vdoc.
