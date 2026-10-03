import os

fn test_owned_map_or_values_acquire_independent_storage() {
	root := os.join_path(os.vtmp_dir(), 'owned_map_or_value_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, '@[has_globals]
module main

__global next_id = 0
__global clones = 0
__global drops = map[int]bool{}
__global keys = 0
__global fallbacks = 0

struct Entry implements IClone, Drop {
 id int
mut:
 text string
}

fn fresh(text string) Entry {
 next_id++
 return Entry{id: next_id, text: text}
}

fn (entry &Entry) clone() Entry {
 clones++
 return fresh(entry.text.clone())
}

fn (mut entry Entry) drop() {
 assert !drops[entry.id], "entry dropped twice"
 drops[entry.id] = true
 entry.text = ""
}

fn key() string {
 keys++
 return "found".repeat(1)
}

fn fallback() []Entry {
 fallbacks++
 return []Entry{}
}

struct Builder {
mut:
 entries map[string][]Entry
}

fn (mut builder Builder) add(text string) {
 name := key()
 mut items := builder.entries[name] or { fallback() }
 items << fresh(text)
 builder.entries[name] = items
}

fn retained(entries &map[string][]Entry) []Entry {
 return entries[key()] or { fallback() }
}

fn byte_copy(entries &map[string][]u8) []u8 {
 return entries["found"] or { []u8{} }
}

fn move_indexed() string {
 mut entries := {"found": "moved".to_owned()}
 address := unsafe { usize(entries["found"].str) }
 item := entries["found"] or { "fallback".to_owned() }
 assert unsafe { usize(item.str) } == address
 return item
}

fn optional_copy(entries &map[string]?[]u8, name string) []u8 {
 return entries[name] or { byte_fallback() }
}

fn optional_first(entries &map[string]?[]u8) u8 {
 value := entries["found"] or { panic("expected original") }
 return value[0]
}

fn byte_fallback() []u8 {
 fallbacks++
 return [u8(9)]
}

fn main() {
 {
  mut builder := Builder{entries: map[string][]Entry{}}
  builder.add("first".repeat(1))
  assert clones == 0
  assert drops.len == 0
  builder.add("second".repeat(1))
  assert clones == 1
  assert drops.len == 1
  builder.add("third".repeat(1))
  assert clones == 3
  assert drops.len == 3
  assert keys == 3
  assert fallbacks == 1
  mut copied := retained(&builder.entries)
  assert keys == 4
  assert fallbacks == 1
  assert clones == 6
  assert copied.len == 3
  assert copied[0].text == "first"
  assert copied[1].text == "second"
  assert copied[2].text == "third"
  assert unsafe { usize(copied.data) != usize(builder.entries["found"].data) }
  copied[0].text = "changed".repeat(1)
  assert builder.entries["found"][0].text == "first"
  drop_owned(copied)
  assert drops.len == 6
 }
 assert drops.len == 9
 assert drops.len == next_id
 {
  bytes := {"found": [u8(1), 2, 3]}
  mut copied := byte_copy(&bytes)
  assert unsafe { usize(copied.data) != usize(bytes["found"].data) }
  copied[0] = 4
  assert bytes["found"] == [u8(1), 2, 3]
 }
 {
  before := clones
  moved := move_indexed()
  assert moved == "moved"
  assert clones == before
 }
 assert drops.len == next_id
 assert fallbacks == 1
 {
  mut entries := map[string]?[]u8{}
  entries["found"] = [u8(1), 2, 3]
  entries["none"] = none
  mut found := optional_copy(&entries, "found")
  found[0] = 8
  assert optional_first(&entries) == 1
  assert fallbacks == 1
  missing := optional_copy(&entries, "missing")
  assert missing == [u8(9)]
  assert fallbacks == 2
  absent := optional_copy(&entries, "none")
  assert absent == [u8(9)]
  assert fallbacks == 3
 }
 println("ok")
}
')!
	for mode in ['-no-parallel', ''] {
		output := os.join_path(root, 'program_${if mode.len > 0 { 'serial' } else { 'parallel' }}')
		compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -ownership -cc clang ${mode} -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
		assert compile.exit_code == 0, '${mode}: ${compile.output}'
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, '${mode}: ${run.output}'
		assert run.output.trim_space() == 'ok', run.output
	}
}
