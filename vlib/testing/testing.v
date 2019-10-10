module testing
/*
asserts for V's basic types:

testing.is(expected, actual, reason) bool               // tests equality
testing.is_not(expected, actual, reason) bool           // tests inequality
testing.is_match(expected, actual, reason) bool         // partial matching string | only on string types

implement for all specific types (until we have generics)
bool
string
i8    i16  int  i64
byte  u16  u32  u64
f32 f64

// maybe add (convenience functions) 
testing.is_before(expected, actual, reason) bool   // equal types:  time  [synonymn for is_less_than and is_greater_than]
testing.is_after(expected, actual, reason) bool   // equal types:  time
...

*/

fn is(expected string, actual string, reason string) bool {
        if expected != actual {
                print_output(expected, actual, reason)
                return false
        }
        return true
}

fn is_not(expected string, actual string, reason string) bool {
        if expected == actual {
                print_output('not `$expected`', actual, reason)
                return false
        }
        return true
}

fn is_match(expected string, actual string, reason string) bool {
        if !actual.contains(expected) {
                print_output('`$expected` to be in the actual string', actual, reason)
                return false
        }
        return true
}

fn print_output(expected, actual, reason string){           
        println('expected: $expected')
        println('actual  : $actual')
        println('reason  : $reason')          
}