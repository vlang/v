## Description

`strings` provides utilities for efficiently processing large strings.

If you got here looking for methods available on the `string` struct, those
methods are found in the `builtin` module.

## Owning string buffer

On the C backend, `Buffer` accumulates bytes and transfers its allocation to a string with
`take_string()`. The transfer does not copy or shrink the accumulated bytes. It reserves a
trailing NUL byte separately, so the result is NUL-terminated even at exact payload capacity.
Embedded NUL bytes, UTF-8, and arbitrary byte sequences are preserved.

```v
import strings

mut buffer := strings.new_buffer(64)
buffer.write_string('hello')
buffer.write_u8(` `)
buffer.write_string('world')
text := buffer.take_string()
assert text == 'hello world'
assert buffer.len() == 0
assert buffer.capacity() == 0
```

`new_buffer(0)` and `Buffer{}` allocate nothing until a write or a positive reservation.
`reserve(additional)` ensures space for that many bytes after the current content without
changing its length. `write(data)` implements `io.Writer`; `write_string` and `write_u8`
append strings and individual bytes. `set_byte(index, value)` replaces an initialized byte
and panics outside the live range. Negative capacities/counts and overflowing lengths panic.

An active buffer owns its allocation exclusively and is marked `nocopy`. Pass it by mutable
reference and do not copy it. It exposes no borrowed strings or byte slices; growing it can
move its allocation.
`write_ptr` and `set_byte_unchecked` are unsafe operations with documented source and index
requirements. In particular, `write_ptr` must not borrow the buffer's own allocation.

After `take_string()`, the returned string owns the original base allocation and may retain
spare capacity. The empty buffer can be reused or freed without affecting previous results.
Under manual memory management, free returned strings and any allocation still held by a
buffer independently. `unsafe { buffer.free() }` releases only its current allocation and
leaves it empty and reusable. The copying `Builder.str()` API remains available when keeping
and reusing the builder's capacity is preferable.

Allocation follows V's current allocator. When using `arena`, a transferred string keeps the
allocation's arena lifetime: it remains valid after `pop()`, but not after `reset()` or `free()`.
Use `clone()` outside that arena when a string must outlive it. `take_string()` does not promote
arena memory to the heap.

## Other string utilities

`pad_left(s, width, pad)`, `pad_right(s, width, pad)`, and `center(s, width, pad)` extend a
string to the requested rune count. They repeat multi-rune padding and truncate it on the right
without splitting UTF-8 sequences. Empty padding, nonpositive widths, and widths no greater
than the input rune count return the input unchanged. `center` puts any extra padding on the
right and restarts the padding pattern on each side: `center('ab', 5, 'xy')` is `'xabxy'`.
Widths count runes, not grapheme clusters or terminal display columns.

`zfill(s, width)` adds zeros on the left, after a leading ASCII `+` or `-` if present.
It includes that sign in the requested rune count and leaves the original input bytes intact.
For example, `zfill('-42', 5)` is `'-0042'`; the input does not have to be numeric.

`cut(s, sep)` returns the text before and after the first exact byte sequence `sep`,
plus a boolean indicating a match. An absent separator returns `s`, `''`, and `false`;
an empty separator returns `''`, `s`, and `true`, even when `s` is empty.
`cut_prefix(s, prefix)` and `cut_suffix(s, suffix)` remove at most one matching affix
and return the remaining string plus the match boolean. An empty affix always matches
without changing `s`. These helpers preserve UTF-8 and arbitrary byte content as supplied.

Builder rune methods write U+FFFD for surrogate halves and values above U+10FFFF,
including repeated writes. Valid Unicode scalar values retain their UTF-8 encoding.

`levenshtein_distance_percentage` returns 100 for identical strings, including two empty strings.
An empty string compared with a nonempty string returns zero.

`get_close_matches(word, candidates, cutoff)` ranks candidates by Levenshtein similarity
from highest to lowest, retaining candidate order for tied scores. The required cutoff
is on a 0..100 scale; NaN and values outside that range panic. For example,
`get_close_matches('appel', ['apple', 'pear', 'ape'], 60)` returns `['apple', 'ape']`.

Builder buffer methods clamp requested byte ranges to the stored buffer. Negative counts to
`go_back` or `cut_last` do nothing; counts beyond its length discard or return the whole buffer.
`go_back_to` and `cut_to` treat negative positions as zero and leave positions past the end alone.
`spart` returns only the intersection of the requested range with the buffer.

`Builder.swap(mut other)` exchanges contents, capacity, and storage metadata without
allocating or copying bytes. Both builders remain usable, including when they are slice views.
Swapping a builder with itself does nothing.
The storage exchange also applies when compiling with `-autofree`.

`source.transfer_to(mut target)` appends all source bytes and empties the source. When the
target is empty, it swaps storage, so the source receives the target's empty buffer. Otherwise,
it appends directly without an intermediate string and clears the source for reuse. Independent
builders retain the source's capacity; shared storage follows array slice detachment rules.
Detached targets own their new storage and release replaced buffers when growing with `-gc none`.
When an empty target aliases the source, the source receives a fresh zero-capacity builder
instead of the target's alias, so reusing the source cannot overwrite the transferred bytes.
Self-transfer does nothing, and an empty source leaves both buffers unchanged. Unlike
`target.drain_builder(mut source, cap)`, transfer does not free and recreate the source buffer.

`split_after(s, sep)` retains each separator in its preceding piece, including a final empty
piece after a trailing separator. An empty separator returns `[s]`. `equal_fold(a, b)` folds
ASCII letters only. `last_index_any(s, chars)` searches bytes and returns a byte index or `-1`;
use `contains_rune(s, r)` to search for a complete Unicode rune.

`indent(s, prefix, skip_first)` adds `prefix` to each line separated by `\n`.
When `skip_first` is true, the first line is unchanged. Empty lines are also prefixed,
including the empty final line after a trailing newline. An empty input is one empty line:
it returns `prefix` when `skip_first` is false, and an empty string otherwise.

## Tabular text

`TabWriter` collects rows with `write_row(columns)` and aligns their columns using spaces.
`min_width` is the minimum content width of each column; `padding` is the additional spacing
after it. Their defaults are zero and one, respectively, and negative values are treated as zero.
Widths count Unicode runes, so they do not measure terminal display widths or grapheme clusters.
Cells are copied as supplied; embedded tabs, line breaks, and escape sequences are not interpreted.

Rows can have different lengths; missing cells are treated as empty strings. The final column
is not padded. `render()` joins the rows with newlines without adding an extra final newline,
and leaves the collected rows available for later renders. `write_row` copies the input array,
and `reset()` clears the rows while keeping the configuration. An overflowing column width panics.

```v
import strings

fn main() {
	mut table := strings.TabWriter{}
	table.write_row(['name', 'age'])
	table.write_row(['alice', '30'])
	assert table.render() == 'name  age\nalice 30'
}
```
