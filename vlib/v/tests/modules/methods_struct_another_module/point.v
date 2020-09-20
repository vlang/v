module point

pub struct Point {
    pub mut:
        x int
        y int
}

pub fn (a Point) +(b Point) Point {
    return Point {
        x: a.x + b.x
        y: a.y + b.y
    }
}

pub fn (a Point) str() string {
    return '${a.x} ${a.y}'
}