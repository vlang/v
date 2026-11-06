struct Foo {
    a C.atomic_uintptr_t
    x int
}

fn main() {
    mut p := unsafe { &Foo(malloc(16)) }
    unsafe { p.x = 42 }
    println(p.x)
    C.atomic_store_ptr(unsafe { &voidptr(&p.a) }, unsafe { nil })
    println('ok')
}
