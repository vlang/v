module table

[inline]
fn type_idx(t Type) int {
    return i16(int(t) >> 16) & 0xffffffff
}

[inline]
fn type_nr_muls(t Type) int {
    return i16(int(t) & 0xffffffff)
}

[inline]
fn type_is_ptr(t Type) bool {
    return i16(int(t) & 0xffffffff) > 0
}

[inline]
fn new_type(idx int) Type {
    if idx > 32767 || idx < -32767 {
        panic('new_type_id: idx must be between -32767 & 32767')
    }
    return idx << i16(16)
}

[inline]
fn type_ptr(t Type) Type {
    return type_idx(t) << i16(16) | (type_nr_muls(t)+1)
}

[inline]
fn type_deref(t Type) Type {
    idx := type_idx(t)
    nr_muls := type_nr_muls(t)
    if nr_muls == 0 {
        panic('deref: $idx is not a pointer')
    }
    return idx << i16(16) | (nr_muls+-1)
}

[inline]
fn new_type_ptr(idx, nr_muls int) Type {
    if idx > 32767 || idx < -32767 {
        panic('typ_ptr: idx must be between -32767 & 32767')
    }
    if nr_muls > 32767 || nr_muls < -0 {
        panic('typ_ptr: nr_muls must be between 0 & 32767')
    }
    return idx << i16(16) | nr_muls
}

[inline]
fn type_is_unresolved(t Type) bool {
    return type_idx(t) < 0
}