fn simple<T>(p T) T {
    return p
}

fn sum<T>(l []T, nil T) T {
    mut r := nil
    for e in l {
        r += e
    }
    return r
}

fn map_f<T,U,R>(l []T, f fn(T)U) []U {
    mut r := []U
    for e in l {
        r << f(e)
    }
    return r
}

fn foldl<T>(l []T, nil T, f fn(T,T)T) T {
    mut r := nil
    for e in l {
        r = f(r, e)
    }
    return r
}

fn plus<T>(a T, b T) T {
    return a+b
}

fn square(x int) int {
    return x*x
}

fn mul_int(x int, y int) int {
    return x*y
}

fn assert_eq<T>(a, b T) {
    r := a == b
    println('$a == $b: ${r.str()}')
    assert r
}

fn test_generic_fn() {
    assert_eq(simple(0+1), 1)
    assert_eq(simple('g') + 'h', 'gh')
    assert_eq(sum([5.1,6.2,7.0], 0.0), 18.3)
    assert_eq(plus(i64(4), i64(6)), i64(10))
    a := [1,2,3,4]
    b := map_f(a, square)
    assert_eq(sum(b, 0), 30)     // 1+4+9+16 = 30
    assert_eq(foldl(b, 1, mul_int), 576)   // 1*4*9*16 = 576
}
