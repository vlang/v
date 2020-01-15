struct Foo{
        a string
        b Bar
}

fn test_checker() {
        //mut x = 'hello'
        bb := Bar{a: 122}
        mut a := Foo{
                a: 'hello'
                // b: bb
        }
        // a.c = 'sa'
        a.a = 'da'
        a.b.a = 111


        r1 := bb.testa()

        // xx := a.a
        // c := 1 + 'strring'
        // mut x := testb()
        mut x = 'hello'
        // mut x = 'hello'
        x = 'foo'
        x = testb(111)

        mut bbb := testa()
        bbb = 1

        mut z := 1
        z = 2
}

// fn main() {
//         //println('test')
//         //a := 1 + 'boop'
//         testa()
// }


fn testa() int {
        return 1
}

// fn testb(a int) int {
//         return 1
// }

fn testb(a int) string {
        return 'hello'
}


struct Bar {
        a int
}

fn (f &Foo) testa() int {
        return 4
}

fn (b &Bar) testa() int {
        return 4
}