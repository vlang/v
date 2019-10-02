fn test_int2int() {
    // TODO: implement V syntax for the hashmap
    //
    // mut hmap := hashmap[int]int
    // hmap[3] = 5432
    // hmap[5] = 5433
    // hmap[3] = 5434
    //
    // assert hmap[3] == 5434
    // assert hmap.len == 2

    mut hmap := new_int_hashmap()

    hmap.put(3, voidptr(5432))
    hmap.put(5, voidptr(5433))
    hmap.put(3, voidptr(5434))

    // is the map size correct?
    assert hmap.size() == 2
    // is there a value with key 3
    assert hmap.has(3)
    // does removing an non-existing element return null?
    assert hmap.remove(1) == 0
    // is the map size still correct?
    assert hmap.size() == 2
    // is an existing value correct?
    assert int(hmap.get(5)) == 5432
    // is an overriden value correct?
    assert int(hmap.get(3)) == 5434
    // is an non-existing value null?
    assert int(hmap.get(1)) == 0

    keys := hmap.keys()

    // is number of returned keys equal to hashmap size?
    assert hmap.size() == keys.len

    // remove an existing element and compare it's value
    assert int(hmap.remove(3)) == 5434
    // is the map size correct?
    assert hmap.size() == 1

    // clear the map
    hmap.clear()
    // is the map empty?
    assert hmap.size() == 0

    // insert 1000 values into the map
    for i in 0..1000 {
        hmap.put(i, voidptr(i * 2))
    }

    // does the map have 1000 elements?
    assert hmap.size() == 1000
}