import os

fn fact(x int) int {
    if x == 1 {
        return 1
    } else {
        return x * fact (x - 1)
    }
}

fn main() {
    if os.args.len != 2 {
        println('usage: factorial [rank]')

        // Exit
        return
    }

    stop := os.args[1].int()
    facto := fact(stop)
    println(facto)
}