import hello as greeting

fn JS.alert(arg string)
fn JS.console.log(arg string)

const (
    i_am_a_const = 21214
    super = 'amazing keyword'
)

struct Companies {
	google int
    amazon bool
    yahoo string
}

enum POSITION {
    go_back
    dont_go_back
}

fn class(extends string, instanceof int) {
    delete := instanceof
}


fn main() {
    JS.console.log('Hello from V.js!')

    v := "done"
    {
        _ := "block"
    }

    pos := POSITION.go_back

    debugger := 'JS keyword'
    // TODO: Implement interpolation
    await := super + debugger
    mut finally := 'implemented'

    JS.console.log(await, finally)

    dun := i_am_a_const * 20

    for i := 0; i < 10; i++ {}

    for i, x in 'hello' {}

    for x in 1..10 {}

    arr := [1,2,3,4,5]
    for a in arr {}

    ma := {
        'str':  "done"
        'ddo': "baba"
    }

    for m, n in ma {
        iss := m
    }

    go async(0, "hello")

    fn_in_var := fn (number int) {
        JS.console.log("number: $number")
    }

    greeting := greeting.excited()
    anon_consumer(greeting, fn (message string) {
        JS.console.log(message)
    })
}

fn anon_consumer (greeting string, anon fn(message string)) {
    anon(greeting)
}

fn async(num int, def string) {}

[inline]
fn hello(game_on int, dummy ...string) (int, int) {
    defer {
        do := "not"
    }
    for dd in dummy {
        l := dd
    }
    return game_on + 2, 221
}

fn (it Companies) method() int {

    ss := Companies {
        google: 2
        amazon: true
        yahoo: "hello"
	}

    a, b := hello(2, 'google', 'not google')

    glue := if a > 2 { 'more_glue' } else if a > 5 {'more glueee'} else { 'less glue' }

    if a != 2 {}

    return 0
}
