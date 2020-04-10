struct done {
	google int
    amazon bool
    yahoo string
}

fn main() {
    v := "done"
    {
        blo := "block"
    }
}

[inline]
fn hello(game_on int, dummy ...string) (int, int) {
    return game_on + 2, 221
}

fn (it done) method() {
   a, b := hello(2, 'google', 'not google')
}