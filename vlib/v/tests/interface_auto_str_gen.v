struct Dog { breed string }
struct Cat { breed string }

interface Animal {
	breed string
}

struct Holder {
	x Animal
}

struct Holder2 {
	x map[string]Holder
	breed string
}

a := Animal(Dog{'hi'})
h := Holder{a}
m := map{'dsa': h}
h2 := Holder2{ m, 'N/A' }
a2 := Animal(h2)

assert '$a2' == r"
Animal(Holder2{
    x: {'dsa': Holder{
        x: Animal(Dog{
            breed: 'hi'
        })
    }}
    breed: 'N/A'
})
".trim_space()
