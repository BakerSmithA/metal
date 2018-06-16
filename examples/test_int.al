import test
import int

// effect: tests that actual and expected are equal. If not then the message
//      will be printed.
func assert_int_eq actual:Int expected:Int name:Tape {
    assert_tape_eq actual.bin expected.bin name
}

// Tests checking whether an integer is zero.
func test_is_zero {
    func assert_is_zero num:Int exp:Tape name:Tape {
        let out = ""
        is_zero num out
        assert_tape_eq out exp name
    }

    assert_is_zero (Int "0") "1" "0==0"
    assert_is_zero (Int "1") "0" "0!=1"
    assert_is_zero (Int "000") "1" "000==0"
    assert_is_zero (Int "00000100") "0" "00000100!=0"
}

// Tests addition of binary integers.
func test_add {
    func assert_add x:Int y:Int exp:Int name:Tape {
        let out = Int ""
        add x y out
        assert_int_eq out exp name
    }

    assert_add (Int "0") (Int "0") (Int "00") "0+0"
    assert_add (Int "1") (Int "0") (Int "10") "1+0"
    assert_add (Int "1011") (Int "0110") (Int "11001") "1101+110"
    assert_add (Int "101101011") (Int "010101110") (Int "1110100101") "110101101+011101010"

    assert_add (Int "10") (Int "1") (Int "010") "Adds operands of different lengths"

    let x = Int ("01")
    assert_add x x (Int "001") "Adding same integer as both operands"
}

// Tests the binary subtraction of integers.
func test_sub {
    func assert_sub x:Int y:Int exp:Int name:Tape {
        let out = Int ""
        sub x y out
        assert_int_eq out exp name
    }

    assert_sub (Int "0") (Int "0") (Int "00") "0-0"
    assert_sub (Int "1") (Int "0") (Int "10") "1-0"
    assert_sub (Int "1011") (Int "0110") (Int "11100") "1101-110"
    assert_sub (Int "101101011") (Int "010101110") (Int "1100001100") "110101101-011101010"

    assert_sub (Int "1") (Int "01") (Int "111") "Subs operands of different lengths"

    let x = Int ("01")
    assert_sub x x (Int "000") "Subtracting same integer as both operands"
}

test_is_zero
test_add
test_sub
