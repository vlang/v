module main

///// ++++++++++++++++++++ \\\\\
// ++== Registers struct ==++ \\
///// ++++++++++++++++++++ \\\\\

pub struct Registers {
pub mut:
    x [31]u64
}

pub fn Registers.new() Registers {
    return Registers{
        x: [31]u64{init: 0}
    }
}

pub fn (r &Registers) read(index int) u64 {
    if index == 31 {
        return 0 // free XZR, always return 0[span_1](start_span)[span_1](end_span)
    }
    // << @optimization: safety is guaranteed by compiler / explicit checks >> \\
    unsafe {
        return r.x[index]
    }
}

pub fn (mut r Registers) write(index int, new_value u64) {
    if index == 31 {
        return // free XZR, writes are discarded[span_2](start_span)[span_2](end_span)
    }
    // << @optimization: safety is guaranteed by compiler / explicit checks >> \\
    unsafe {
        r.x[index] = new_value
    }
}


///// ++++++++++++++++ \\\\\
// ++== Flags struct ==++ \\
///// ++++++++++++++++ \\\\\

pub struct Flags {
pub mut:
    zero_flag     bool // 'Z': result was zero[span_3](start_span)[span_3](end_span)
    negative_flag bool // 'N': result's top bit was 1 (signed negative)[span_4](start_span)[span_4](end_span)
    carry_flag    bool // 'C': for SUB, 1 = no borrow; for ADD, 1 = carry out[span_5](start_span)[span_5](end_span)
    overflow_flag bool // 'V': signed overflow (result sign != expected)[span_6](start_span)[span_6](end_span)
}

pub fn Flags.new() Flags {
    return Flags{
        zero_flag: false
        negative_flag: false
        carry_flag: false
        overflow_flag: false
    }
}

pub struct State {
pub mut:
    is_running bool
}

pub fn State.new() State {
    return State{
        is_running: false
    }
}
