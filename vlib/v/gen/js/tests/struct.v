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

fn (i Int) get() int {
  return i.value
}

fn main() {
  a := Int { value: 10 }
  a.add(5)
  println(a) // 15

  b := Int{}
  b.add(10)
  println(b.get()) // 10
}
