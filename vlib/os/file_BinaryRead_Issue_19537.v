// This is the code that is converted from the source code provided in the issue which is converted from Golang to Vlang
import(
    'errors'
    'io'
    'math'
    'reflect'
    'sync'
    'os'
)

pub struct ByteOrder {
    pub fn uint16(b []byte) u16
    pub fn uint32(b []byte) u32
    pub fn uint64(b []byte) u64
}

struct littleEndian {
    mut order ByteOrder
}

pub fn (le littleEndian) uint16(b []byte) u16 {
    return u16(b[0]) | u16(b[1]) << 8
}

pub fn (le littleEndian) uint32(b []byte) u32 {
    return u32(b[0]) | u32(b[1]) << 8 | u32(b[2]) << 16 | u32(b[3]) << 24
}

pub fn (le littleEndian) uint64(b []byte) u64 {
    return u64(b[0]) | u64(b[1]) << 8 | u64(b[2]) << 16 | u64(b[3]) << 24 |
        u64(b[4]) << 32 | u64(b[5]) << 40 | u64(b[6]) << 48 | u64(b[7]) << 56
}

fn read(r io.Reader, order ByteOrder, data interface{}) ?error {
    mut n := int_data_size(data)
    if n != 0 {
        mut bs := []byte{0} * n
        if _, err := io.read_full(r, bs)?; err != null {
            return err
        }
        match data {
            mut data is *bool {
                *data = bs[0] != 0
            }
            mut data is *i8 {
                *data = i8(bs[0])
            }
            mut data is *u8 {
                *data = u8(bs[0])
            }
            mut data is *i16 {
                *data = i16(order.uint16(bs))
            }
            mut data is *u16 {
                *data = order.uint16(bs)
            }
            mut data is *i32 {
                *data = i32(order.uint32(bs))
            }
            mut data is *u32 {
                *data = order.uint32(bs)
            }
            mut data is *i64 {
                *data = i64(order.uint64(bs))
            }
            mut data is *u64 {
                *data = order.uint64(bs)
            }
            mut data is *f32 {
                *data = f32(math.float32frombits(order.uint32(bs)))
            }
            mut data is *f64 {
                *data = f64(math.float64frombits(order.uint64(bs)))
            }
            mut data is []bool {
                for i, x in range bs {
                    data[i] = x != 0
                }
            }
            mut data is []i8 {
                for i, x in range bs {
                    data[i] = i8(x)
                }
            }
            mut data is []u8 {
                copy(data, bs)
            }
            mut data is []i16 {
                for i := range data {
                    data[i] = i16(order.uint16(bs[2*i..]))
                }
            }
            mut data is []u16 {
                for i := range data {
                    data[i] = order.uint16(bs[2*i..])
                }
            }
            mut data is []i32 {
                for i := range data {
                    data[i] = i32(order.uint32(bs[4*i..]))
                }
            }
            mut data is []u32 {
                for i := range data {
                    data[i] = order.uint32(bs[4*i..])
                }
            }
            mut data is []i64 {
                for i := range data {
                    data[i] = i64(order.uint64(bs[8*i..]))
                }
            }
            mut data is []u64 {
                for i := range data {
                    data[i] = order.uint64(bs[8*i..])
                }
            }
            mut data is []f32 {
                for i := range data {
                    data[i] = f32(math.float32frombits(order.uint32(bs[4*i..])))
                }
            }
            mut data is []f64 {
                for i := range data {
                    data[i] = f64(math.float64frombits(order.uint64(bs[8*i..])))
                }
            }
            else {
                n = 0
            }
        }
        if n != 0 {
            return null
        }
    }

    mut v := reflect.value_of(data)
    mut size := -1
    match v.kind {
        case reflect.pointer {
            v = v.elem()
            size = data_size(v)
        }
        case reflect.slice {
            size = data_size(v)
        }
    }
    if size < 0 {
        return errors.new('Read: invalid type ' + reflect.type_of(data).string())
    }
    mut d := &decoder{order: order, buf: []byte{0} * size}
    if _, err := io.read_full(r, d.buf)?; err != null {
        return err
    }
    d.value(v)
    return null
}

fn size(v interface{}) i {
    return data_size(reflect.indirect(reflect.value_of(v)))
}

struct struct_size sync.map

fn data_size(v reflect.value) i {
    match v.kind {
        case reflect.slice {
            mut s := size_of(v.type().elem())
            if s >= 0 {
                return s * v.len()
            }
            return -1
        }
        case reflect.struct {
            mut t := v.type()
            if size, ok := struct_size.load(t); ok {
                return size.(i)
            }
            mut size := size_of(t)
            struct_size.store(t, size)
            return size
        }
        else {
            return size_of(v.type())
        }
    }
}

fn size_of(t reflect.type) i {
    match t.kind {
        case reflect.array {
            mut s := size_of(t.elem())
            if s >= 0 {
                return s * t.len()
            }
        }
        case reflect.struct {
            mut sum := 0
            for i, n in 0..t.num_field() {
                mut s := size_of(t.field(i).type)
                if s < 0 {
                    return -1
                }
                sum += s
            }
            return sum
        }
        case reflect.bool, reflect.u8, reflect.u16, reflect.u32, reflect.u64,
            reflect.i8, reflect.i16, reflect.i32, reflect.i64,
            reflect.f32, reflect.f64, reflect.complex64, reflect.complex128 {
            return t.size() as i
        }
        else {
            return -1
        }
    }
    return -1
}

struct coder {
    mut order ByteOrder
    mut buf []byte
    mut offset int
}

struct decoder coder

fn (mut d *decoder) bool() bool {
    mut x := d.buf[d.offset]
    d.offset++
    return x != 0
}

fn (mut d *decoder) u8() u8 {
    mut x := d.buf[d.offset]
    d.offset++
    return x
}

fn (mut d *decoder) u16() u16 {
    mut x := d.order.uint16(d.buf[d.offset..d.offset+2])
    d.offset += 2
    return x
}

fn (mut d *decoder) u32() u32 {
    mut x := d.order.uint32(d.buf[d.offset..d.offset+4])
    d.offset += 4
    return x
}

fn (mut d *decoder) u64() u64 {
    mut x := d.order.uint64(d.buf[d.offset..d.offset+8])
    d.offset += 8
    return x
}

fn (mut d *decoder) i8() i8 { return i8(d.u8()) }
fn (mut d *decoder) i16() i16 { return i16(d.u16()) }
fn (mut d *decoder) i32() i32 { return i32(d.u32()) }
fn (mut d *decoder) i64() i64 { return i64(d.u64()) }

fn (mut d *decoder) value(v reflect.Value) {
    match v.kind {
        case reflect.array {
            mut l := v.len()
            for i := 0; i < l; i++ {
                d.value(v.index(i))
            }
        }
        case reflect.struct {
            mut t := v.type()
            mut l := v.num_field()
            for i := 0; i < l; i++ {
                if v := v.field(i); v.can_set() || t.field(i).name != '_' {
                    d.value(v)
                } else {
                    d.skip(v)
                }
            }
        }
        case reflect.slice {
            mut l := v.len()
            for i := 0; i < l; i++ {
                d.value(v.index(i))
            }
        }
        case reflect.bool {
            v.set_bool(d.bool())
        }
        case reflect.i8 {
            v.set_i64(i64(d.i8()))
        }
        case reflect.i16 {
            v.set_i64(i64(d.i16()))
        }
        case reflect.i32 {
            v.set_i64(i64(d.i32()))
        }
        case reflect.i64 {
            v.set_i64(d.i64())
        }
        case reflect.u8 {
            v.set_u64(u64(d.u8()))
        }
        case reflect.u16 {
            v.set_u64(u64(d.u16()))
        }
        case reflect.u32 {
            v.set_u64(u64(d.u32()))
        }
        case reflect.u64 {
            v.set_u64(d.u64())
        }
        case reflect.f32 {
            v.set_f64(f64(math.float32frombits(d.u32())))
        }
        case reflect.f64 {
            v.set_f64(f64(math.float64frombits(d.u64())))
        }
        case reflect.complex64 {
            v.set_complex(complex(
                f64(math.float32frombits(d.u32())),
                f64(math.float32frombits(d.u32())),
            ))
        }
        case reflect.complex128 {
            v.set_complex(complex(
                f64(math.float64frombits(d.u64())),
                f64(math.float64frombits(d.u64())),
            ))
        }
    }
}

fn (mut d *decoder) skip(v reflect.Value) {
    d.offset += data_size(v)
}

fn int_data_size(data interface{}) i {
    match data {
        is bool, is i8, is u8, is *bool, is *i8, is *u8 {
            return 1
        }
        is []bool {
            return len(data)
        }
        is []i8 {
            return len(data)
        }
        is []u8 {
            return len(data)
        }
        is i16, is u16, is *i16, is *u16 {
            return 2
        }
        is []i16 {
            return 2 * len(data)
        }
        is []u16 {
            return 2 * len(data)
        }
        is i32, is u32, is *i32, is *u32 {
            return 4
        }
        is []i32 {
            return 4 * len(data)
        }
        is []u32 {
            return 4 * len(data)
        }
        is i64, is u64, is *i64, is *u64 {
            return 8
        }
        is []i64 {
            return 8 * len(data)
        }
        is []u64 {
            return 8 * len(data)
        }
        is f32, is *f32 {
            return 4
        }
        is f64, is *f64 {
            return 8
        }
        is []f32 {
            return 4 * len(data)
        }
        is []f64 {
            return 8 * len(data)
        }
        else {
            return 0
        }
    }
}

pub fn main() {
    mut r := io.new_reader('\x01\x00\x00\x00\x00\x00\x00\x00'.to_byte_array())
    mut data := i32(0)
    mut order := littleEndian{order: ByteOrder{}}

    mut err := read(r, order, &data)
    if err != null {
        println('Error: $err')
    } else {
        println(data)
    }
}
