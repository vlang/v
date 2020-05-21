module main

struct Int {
mut:
  value int
  test map[string]int
  hello []int
}

fn (mut i Int) add(value int) {
  i.value += value
}

fn (i Int) get(value int) int {
  return i.value
}

fn main() {
  i := Int { value: 10 }
  i.add(5)
}
