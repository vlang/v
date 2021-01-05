type Test = map[string]string

fn (t Test) get(key string) string {
    return t[key]
}

fn main() {
    t := Test({'c': 'abc'})
    r := t.get('c')
    println(r)
}
