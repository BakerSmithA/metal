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

    // Tests basic adding of integers with same number of bits.
    func test_same_len_operands {
        assert_add (Int "0") (Int "0") (Int "00") "0+0"
        assert_add (Int "1") (Int "0") (Int "10") "1+0"
        assert_add (Int "1011") (Int "0110") (Int "11001") "1101+110"
        assert_add (Int "101101011") (Int "010101110") (Int "1110100101") "110101101+011101010"
    }
    test_same_len_operands

    // Tests using the same integer as both operands.
    func test_same_operand {
        let x = Int "01"
        assert_add x x (Int "001") "Adding same integer as both operands"
    }
    test_same_operand

    // Tests writing to one of the input operands.
    func test_same_out {
        let x = Int "01"
        add x (Int "10") x
        assert_int_eq x (Int "110") "Using integer as input and output"
    }
    test_same_out

    // Tests adding when both inputs and the output are the same integer.
    func test_all_same_operands {
        let x = Int "10"
        add x x x
        assert_int_eq x (Int "010") "Using integer as all inputs and output"
    }
    test_all_same_operands

    // Tests performing one addition followed by another.
    func test_multiple_adds {
        let x = Int "10"
        let y = Int "10"

        let r1 = Int ""
        let r2 = Int ""

        add x y r1
        add r1 y r2

        assert_int_eq r2 (Int "110") "Chaining additions"
    }
    test_multiple_adds
}

// Tests the binary subtraction of integers.
func test_sub {
    func assert_sub x:Int y:Int exp:Int name:Tape {
        let out = Int ""
        sub x y out
        assert_int_eq out exp name
    }

    // Tests basic subtraction of integers with the same number of bits.
    func test_same_len_operands {
        assert_sub (Int "0") (Int "0") (Int "00") "0-0"
        assert_sub (Int "1") (Int "0") (Int "10") "1-0"
        assert_sub (Int "1011") (Int "0110") (Int "11100") "1101-110"
        assert_sub (Int "101101011") (Int "010101110") (Int "1100001100") "110101101-011101010"
    }
    test_same_len_operands

    // Tests using the same integer as both operands.
    func test_same_operand {
        let x = Int ("01")
        assert_sub x x (Int "000") "Subtracting same integer as both operands"
    }
    test_same_operand

    // Tests writing to one of the input operands.
    func test_same_out {
        let x = Int "11"
        sub x (Int "10") x
        assert_int_eq x (Int "010") "Using integer as input and output"
    }
    test_same_out

    // Tests subtracting when both inputs and the output are the same integer.
    func test_all_same_operands {
        let x = Int "10"
        sub x x x
        assert_int_eq x (Int "000") "Using integer as all inputs and output"
    }
    test_all_same_operands

    // Tests performing one subtraction followed by another.
    func test_multiple_subs {
        let x = Int "11"
        let y = Int "10"

        let r1 = Int ""
        let r2 = Int ""

        sub x y r1
        sub r1 y r2

        assert_int_eq r2 (Int "100") "Chaining subtractions"
    }
    test_multiple_subs
}

test_is_zero
test_add
test_sub
