import test
import int

// effect: tests that actual and expected are equal. If not then the message
//      will be printed.
func assert_int_eq actual:Int expected:Int name:Tape {
    assert_tape_eq actual.bin expected.bin name
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
}

test_add
test_sub
