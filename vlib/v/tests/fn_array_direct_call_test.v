struct Placeholder {
    name string
}

struct FnStruct {
mut:
    array_of_fn []fn(int, &Placeholder, string)bool
}

fn test_fn_array_direct_call() {
    mut fs := FnStruct{}
    fs.array_of_fn << fn(x int, y &Placeholder, z string) bool {
        return false
    }

    assert fs.array_of_fn[0](1, &Placeholder{name: 'Bob'}, 'Builder') == false
}
