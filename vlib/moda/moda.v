module moda

pub fn aaaa<T>(x T, indent int) {
    mut space := ''
    for _ in 0..indent {
        space = space + ' '
    }
    println('$space$x')
}
