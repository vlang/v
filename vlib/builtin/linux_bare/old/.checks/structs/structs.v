module main
import forkedtest

struct SimpleEmptyStruct{
}

struct NonEmptyStruct{
  x int
  y int
  z int
}

fn check_simple_empty_struct(){  
  s := SimpleEmptyStruct{}
  addr_s := &s
  str_addr_s := ptr_str( addr_s )
  assert !isnil(addr_s)
  assert str_addr_s.len > 3
  println(str_addr_s)
}

fn check_non_empty_struct(){  
  a := NonEmptyStruct{1,2,3}
  b := NonEmptyStruct{4,5,6}
  assert sizeof(NonEmptyStruct) > 0
  assert sizeof(SimpleEmptyStruct) < sizeof(NonEmptyStruct)
  assert a.x == 1
  assert a.y == 2
  assert a.z == 3
  assert b.x + b.y + b.z == 15
  assert ptr_str(&a) != ptr_str(&b)
  println('sizeof SimpleEmptyStruct:' + i64_str( sizeof(SimpleEmptyStruct) , 10 ))
  println('sizeof NonEmptyStruct:' + i64_str( sizeof(NonEmptyStruct) , 10 ))
}

fn main(){
	mut fails := 0
	fails += forkedtest.normal_run(check_simple_empty_struct, "check_simple_empty_struct")
	fails += forkedtest.normal_run(check_non_empty_struct,    "check_non_empty_struct")
	assert fails == 0
	sys_exit(0)
}
