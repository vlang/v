module testing

// is compares int for equality, printing the values if they differ 
pub fn is(expected int, actual int, reason string) bool {
        if expected != actual {
                print_output(expected.str(), actual.str(), reason)
                return false
        }
        return true
}

// is compares int for inequality, printing the values if they differ 
pub fn is_not(expected int, actual int, reason string) bool {
        if expected == actual {
                print_output('not `$expected`', actual.str(), reason)
                return false
        }
        return true
}

// is_string compares string for equality, printing the values if they differ 
pub fn is_string(expected string, actual string, reason string) bool {
        if expected != actual {
                print_output(expected, actual, reason)
                return false
        }
        return true
}

// is_not_string compares string for inequality, printing the values if they differ 
pub fn is_not_string(expected string, actual string, reason string) bool {
        if expected == actual {
                print_output('not `$expected`', actual, reason)
                return false
        }
        return true
}

// is_match checks if the expected string is found in the actual, printing the values if they differ 
pub fn is_match(expected string, actual string, reason string) bool {
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