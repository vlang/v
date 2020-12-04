module main

import shapes { Point, Line }

// test that Point & Line work correctly
// with struct init & array's
fn test_imported_symbols() {
    p0 := Point  {x: 10 y: 10}
    p1 := Point  {x: 50 y: 10}

    _ := Line {
        ps: [p0, p1]
    }

}