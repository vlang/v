module moda

struct APoint {
mut:
    x f64
    y f64
}

fn (p mut APoint) translate<T>(x, y T) {
    p.x += x
    p.y += y
}


pub fn aaaa<T>(x T, indent int) {
    mut space := ''
    for _ in 0..indent {
        space = space + ' '
    }
    println('$space$x')
}
