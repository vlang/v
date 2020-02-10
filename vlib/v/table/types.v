module table

pub type Type int

// return underlying TypeSymbol idx
[inline]
pub fn type_idx(t Type) int {
    return i16(int(t) >> 16) & 0xffffffff
}

// return nr_muls
[inline]
pub fn type_nr_muls(t Type) int {
    return i16(int(t) & 0xffffffff)
}

// return true if pointer (nr_muls>0)
[inline]
pub fn type_is_ptr(t Type) bool {
    return type_nr_muls(t) > 0
}

// increments nr_nuls on Type and return it
[inline]
pub fn type_to_ptr(t Type) Type {
    return type_idx(t) << i16(16) | (type_nr_muls(t)+1)
}

// decrement nr_muls on Type and return it
[inline]
pub fn type_deref(t Type) Type {
    idx := type_idx(t)
    nr_muls := type_nr_muls(t)
    if nr_muls == 0 {
        panic('deref: $idx is not a pointer')
    }
    return idx << i16(16) | (nr_muls+-1)
}

// new type with idx of TypeSymbol, not pointer (nr_muls=0)
[inline]
pub fn new_type(idx int) Type {
    if idx > 32767 || idx < -32767 {
        panic('new_type_id: idx must be between -32767 & 32767')
    }
    return idx << i16(16)
}

// return Type idx of TypeSymbol & specify if ptr (nr_muls)
[inline]
pub fn new_type_ptr(idx, nr_muls int) Type {
    if idx > 32767 || idx < -32767 {
        panic('typ_ptr: idx must be between -32767 & 32767')
    }
    if nr_muls > 32767 || nr_muls < -0 {
        panic('typ_ptr: nr_muls must be between 0 & 32767')
    }
    return idx << i16(16) | nr_muls
}

// true if the type of unresolved expression
[inline]
pub fn type_is_unresolved(t Type) bool {
    return type_idx(t) < 0
}

pub const (
    void_type    = new_type(void_type_idx)
    voidptr_type = new_type(voidptr_type_idx)
    byteptr_type = new_type(byteptr_type_idx)
    charptr_type = new_type(charptr_type_idx)
    i8_type      = new_type(i8_type_idx)
    int_type     = new_type(int_type_idx)
    i16_type     = new_type(i16_type_idx)
    i64_type     = new_type(i64_type_idx)
    byte_type    = new_type(byte_type_idx)
    u16_type     = new_type(u16_type_idx)
    u32_type     = new_type(u32_type_idx)
    u64_type     = new_type(u64_type_idx)
    f32_type     = new_type(f32_type_idx)
    f64_type     = new_type(f64_type_idx)
    char_type    = new_type(char_type_idx)
    bool_type    = new_type(bool_type_idx)
    string_type  = new_type(string_type_idx)
    array_type   = new_type(array_type_idx)
    map_type     = new_type(map_type_idx)
)
