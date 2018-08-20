import bf_interpreter
import test

// Tests the Brainfuck interpreter.
proc test_interpreter {
    proc assert_inter instrs:Tape expected:Tape name:Tape {
        let out = ""
        interpret instrs (Counter "12345678") '0' out
        assert_tape_eq out expected name
    }

    assert_inter "" "" "nothing"
    assert_inter "++++++++++++++++++++++++++++++++++++++++++++++++.+.+.+.+.+." "012345" "incrementing"
    assert_inter "++++++[->++++++++++++<]>.----[--<+++>]<-.+++++++..+++.[--->+<]>-----.--" "Hello" "Hello"
    assert_inter "++++++[->++++++++++++<]>.----[--<+++>]<-.+++++++..+++.[--->+<]>-----.---[-<+++>]<.---[--->++++<]>-.+++.------.--------.-[---<+>]<.[--->+<]>-." "Hello World!" "Hello_World!"
}
test_interpreter
