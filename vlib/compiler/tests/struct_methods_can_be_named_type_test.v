struct Mystruct { 
   x int 
}

fn (s Mystruct) int() int { 
   return s.x 
}
fn (s Mystruct) str() string {
  return 'Mystruct, x=$s.x' 
}

fn test_int_can_be_called_on_receivers_that_define_it() {
   s := Mystruct{123}
   assert s.int() == 123
   assert s.str() == 'Mystruct, x=123'
}
