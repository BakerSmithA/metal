import test
import logic

// Tests the bin_not function.
proc test_bin_not {
    proc assert_not p:Sym exp:Tape name:Tape {
        let out = ""
        bin_not p out
        assert_tape_eq out exp name
    }

    assert_not '0' "1" "not 0"
    assert_not '1' "0" "not 1"
}

// Tests the bin_or function.
proc test_bin_or {
    proc assert_or p:Sym q:Sym exp:Tape name:Tape {
        let out = ""
        bin_or p q out
        assert_tape_eq out exp name
    }

    assert_or '0' '0' "0" "0 or 0"
    assert_or '0' '1' "1" "0 or 1"
    assert_or '1' '0' "1" "1 or 0"
    assert_or '1' '1' "1" "1 or 1"
}

// Tests bin_and function.
proc test_bin_and {
    proc assert_and p:Sym q:Sym exp:Tape name:Tape {
        let out = ""
        bin_and p q out
        assert_tape_eq out exp name
    }

    assert_and '0' '0' "0" "0 and 0"
    assert_and '0' '1' "0" "0 and 1"
    assert_and '1' '0' "0" "1 and 0"
    assert_and '1' '1' "1" "1 and 1"
}

// Tests bin_xor function.
proc test_bin_xor {
    proc assert_xor p:Sym q:Sym exp:Tape name:Tape {
        let out = ""
        bin_xor p q out
        assert_tape_eq out exp name
    }

    assert_xor '0' '0' "0" "0 xor 0"
    assert_xor '0' '1' "1" "0 xor 1"
    assert_xor '1' '0' "1" "1 xor 0"
    assert_xor '1' '1' "0" "1 xor 1"
}

// Tests the binary half adder.
proc test_bin_half_adder {
    proc assert_adder p:Sym q:Sym exp:Tape name:Tape {
        let out = ""
        bin_half_adder p q out
        assert_tape_eq out exp name
    }

    assert_adder '0' '0' "00" "half_adder 0 0"
    assert_adder '0' '1' "10" "half_adder 0 1"
    assert_adder '1' '0' "10" "half_adder 1 0"
    assert_adder '1' '1' "01" "half_adder 1 1"
}

// Tests the binary full adder.
proc test_bin_full_adder {
    proc assert_adder p:Sym q:Sym c_in:Sym exp:Tape name:Tape {
        let out = ""
        bin_full_adder p q c_in out
        assert_tape_eq out exp name
    }

    assert_adder '0' '0' '0' "00" "full_adder 0 0 0"
    assert_adder '0' '0' '1' "10" "full_adder 0 0 1"
    assert_adder '0' '1' '0' "10" "full_adder 0 1 0"
    assert_adder '0' '1' '1' "01" "full_adder 0 1 1"
    assert_adder '1' '0' '0' "10" "full_adder 1 0 0"
    assert_adder '1' '0' '1' "01" "full_adder 1 0 1"
    assert_adder '1' '1' '0' "01" "full_adder 1 1 0"
    assert_adder '1' '1' '1' "11" "full_adder 1 1 1"
}

// Tests the binary half subtractor.
proc test_bin_half_sub {
    proc assert_sub p:Sym q:Sym exp:Tape name:Tape {
        let out = ""
        bin_half_sub p q out
        assert_tape_eq out exp name
    }

    assert_sub '0' '0' "00" "half_sub 0 0"
    assert_sub '0' '1' "11" "half_sub 0 1"
    assert_sub '1' '0' "10" "half_sub 1 0"
    assert_sub '1' '1' "00" "half_sub 1 1"
}

// Tests the binary full subtractor.
proc test_bin_full_sub {
    proc assert_sub b_in:Sym y:Sym x:Sym exp:Tape name:Tape {
        let out = ""
        bin_full_sub x y b_in out
        assert_tape_eq out exp name
    }

    assert_sub '0' '0' '0' "00" "full_sub 0 0"
    assert_sub '0' '0' '1' "10" "full_sub 0 0"
    assert_sub '0' '1' '0' "11" "full_sub 0 0"
    assert_sub '0' '1' '1' "00" "full_sub 0 0"
    assert_sub '1' '0' '0' "11" "full_sub 0 0"
    assert_sub '1' '0' '1' "00" "full_sub 0 0"
    assert_sub '1' '1' '0' "01" "full_sub 0 0"
    assert_sub '1' '1' '1' "11" "full_sub 0 0"
}

test_bin_not
test_bin_or
test_bin_and
test_bin_xor
test_bin_half_adder
test_bin_full_adder
test_bin_half_sub
test_bin_full_sub
