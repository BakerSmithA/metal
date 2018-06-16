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
    assert_add (Int "10") (Int "01") (Int "110") "10+01"
    assert_add (Int "110") (Int "111") (Int "0101") "011+111"
}

test_add
