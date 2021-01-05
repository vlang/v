type Test = map[string]string

fn test_index() {
    t := Test({'c': 'abc'})
    assert t['c'] == 'abc'
}
