interface Animal { name string }
struct Dog { name string }
struct Cat { name string }

fn test_interface_match() {
    a := Animal(Dog{name: 'Jet'})
    match a {
        Dog { assert true }
        Cat { assert false }
        else { assert false }
    }
}
