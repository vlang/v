module c

import os

fn test_sumtype_borrowed_reference_conditionals_preserve_identity() {
	dir := os.join_path(os.vtmp_dir(), 'sumtype_reference_conditional_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	path := os.join_path(dir, 'main.v')
	os.write_file(path, "struct ConditionalRefLeaf {
    value int
}

struct ConditionalRefWrapper {
    target &ConditionalRefSum
}

type ConditionalRefSum = ConditionalRefLeaf | ConditionalRefWrapper

@[noinline]
fn conditional_choose_reference(flag bool, left &ConditionalRefSum, right &ConditionalRefSum) &ConditionalRefSum {
    return if flag { left } else { right }
}

@[noinline]
fn conditional_local_reference(typ &ConditionalRefSum) &ConditionalRefSum {
    clean := if typ is ConditionalRefWrapper { typ.target } else { typ }
    return clean
}

fn conditional_identity(value &ConditionalRefSum) &ConditionalRefSum {
    return value
}

@[noinline]
fn conditional_forward_reference(typ &ConditionalRefSum) &ConditionalRefSum {
    if typ is ConditionalRefWrapper {
        return conditional_identity(typ.target)
    }
    return conditional_identity(typ)
}

@[noinline]
fn conditional_block_reference(flag bool, left &ConditionalRefSum, right &ConditionalRefSum) &ConditionalRefSum {
    clean := if flag {
        assert left is ConditionalRefLeaf
        left
    } else {
        assert right is ConditionalRefLeaf
        right
    }
    return clean
}

@[noinline]
fn conditional_nested_reference(index int, first &ConditionalRefSum, second &ConditionalRefSum, third &ConditionalRefSum) &ConditionalRefSum {
    return if index == 0 { first } else if index == 1 { second } else { third }
}

@[noinline]
fn conditional_explicit_value_control(typ &ConditionalRefSum) ConditionalRefSum {
    return if typ is ConditionalRefWrapper { *typ.target } else { *typ }
}

fn conditional_ref_value(typ &ConditionalRefSum) int {
    if typ is ConditionalRefLeaf {
        return typ.value
    }
    return -1
}

fn main() {
    mut first := ConditionalRefSum(ConditionalRefLeaf{value: 41})
    second := ConditionalRefSum(ConditionalRefLeaf{value: 73})
    third := ConditionalRefSum(ConditionalRefLeaf{value: 89})
    wrapper := ConditionalRefSum(ConditionalRefWrapper{target: &first})
    assert voidptr(conditional_choose_reference(true, &first, &second)) == voidptr(&first)
    assert voidptr(conditional_choose_reference(false, &first, &second)) == voidptr(&second)
    assert voidptr(conditional_local_reference(&first)) == voidptr(&first)
    assert voidptr(conditional_local_reference(&wrapper)) == voidptr(&first)
    assert voidptr(conditional_forward_reference(&first)) == voidptr(&first)
    assert voidptr(conditional_forward_reference(&wrapper)) == voidptr(&first)
    assert voidptr(conditional_block_reference(true, &first, &second)) == voidptr(&first)
    assert voidptr(conditional_block_reference(false, &first, &second)) == voidptr(&second)
    assert voidptr(conditional_nested_reference(0, &first, &second, &third)) == voidptr(&first)
    assert voidptr(conditional_nested_reference(1, &first, &second, &third)) == voidptr(&second)
    assert voidptr(conditional_nested_reference(2, &first, &second, &third)) == voidptr(&third)
    selected := conditional_local_reference(&wrapper)
    copied := conditional_explicit_value_control(&wrapper)
    first = ConditionalRefSum(ConditionalRefLeaf{value: 97})
    assert conditional_ref_value(selected) == 97
    assert conditional_ref_value(conditional_forward_reference(&wrapper)) == 97
    assert conditional_ref_value(&copied) == 41
    assert conditional_ref_value(conditional_choose_reference(false, &first, &second)) == 73
    println('borrowed-reference conditionals ok')
}
")!
	result := os.exec([@VEXE, '-new-compiler', '-nocache', 'run', path])
	assert result.exit_code == 0, result.output
	assert result.output.contains('borrowed-reference conditionals ok'), result.output
}
