
const (
    i_am_a_const = 21214
)

struct Companies {
	google int
    amazon bool
    yahoo string
}

enum POSITION {
    GO_BACK,
    DONT_GO_BACK
}

fn main() {
    v := "done"
    {
        _ := "block"
    }

    for i := 0; i < 10; i++ {}

    for i, x in 'hello' {}

    for x in 1..10 {}

    arr := [1,2,3,4,5]
    for a in arr {}

    ma := map[string]string
}

[inline]
fn hello(game_on int, dummy ...string) (int, int) {
    defer {
        do := "not"
    }
    return game_on + 2, 221
}

fn (it done) method() {

    ss := Companies {
        google: 2
        amazon: true
        yahoo: "hello"
	}

   a, b := hello(2, 'google', 'not google')
}
