module main

const (
   integer1 = 111
   integer2 = 222
   integer3 = 333
   integer9 = integer3 * 3
   abc = "123"
)

fn main(){
   assert abc == "123"
   assert integer9 == 999
   println("constants are properly initialized")
}
