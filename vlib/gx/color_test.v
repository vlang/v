import gx


fn test_hex() {
    // valid colors
    a := gx.hex(0x6c5ce7ff)
    b := gx.rgba(108, 92, 231, 255)

    assert a.eq(b)

    // doesn't give right value with short hex value
    short := gx.hex(0xfff)
    
    assert !short.eq(gx.white)
}

fn test_to_hex() {
    // valid colors
    a := 0x6c5ce7ff //normal hex    
    b := 0xff6c5ce7 //swapped hex
    c := gx.rgba(108, 92, 231, 255).to_hex
    d := gx.rgba(108, 92, 231, 255).to_hex_swap //used in some gpu system
    
    assert a==c
}
fn test_to_hex_swap() {
    // valid colors
    a := 0x6c5ce7ff //normal hex    
    b := 0xff6c5ce7 //swapped hex
    c := gx.rgba(108, 92, 231, 255).to_hex
    d := gx.rgba(108, 92, 231, 255).to_hex_swap //used in some gpu system    
    assert d==b   
}

fn test_add() {
    a := gx.rgba(100, 100, 100, 100)
    b := gx.rgba(100, 100, 100, 100)
    r := gx.rgba(200, 200, 200, 200)

    assert (a + b).eq(r)
}

fn test_sub() {
    a := gx.rgba(100, 100, 100, 100)
    b := gx.rgba(100, 100, 100, 100)
    r := gx.rgba(0, 0, 0, 0)

    assert (a - b).eq(r)
}

fn test_mult() {
    a := gx.rgba(10, 10, 10, 10)
    b := gx.rgba(10, 10, 10, 10)
    r := gx.rgba(100, 100, 100, 100)

    assert (a * b).eq(r)
}

fn test_div() {
    a := gx.rgba(100, 100, 100, 100)
    b := gx.rgba(10, 10, 10, 10)
    r := gx.rgba(10, 10, 10, 10)

    assert (a / b).eq(r)
}
