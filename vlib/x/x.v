module x

fn main() {
    $if plan9 {
        println('compiled for plan9')
    } $else {
        println('another os')
    }
    println('ok')
}

