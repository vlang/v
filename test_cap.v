fn main() {
    ch := chan int{cap: 5}
    println(ch.cap)
}
