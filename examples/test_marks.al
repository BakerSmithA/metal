import marks
import test

// Tests marking the tape writes the next free symbol to the tape.
proc test_mark {
    let ms = Marks "" ""
    init_marks ms "1234"

    // Symbol used to mark the tape.
    let mark_sym = read (ms.free)

    // Mark the testing tape.
    let t = "abcd"
    mark t ms

    let expected = "abcd"
    // Write the expected marking symbol to the expected tape.
    write expected mark_sym

    assert_tape_eq t expected "mark_tape"
}
test_mark

// Tests marking and unmarking the tape has no overall effect.
proc test_mark_unmark {
    let ms = Marks "" ""
    init_marks ms "1234"

    // Mark the testing tape.
    let t = "abcd"
    mark t ms
    unmark t ms

    assert_tape_eq t "abcd" "mark_unmark_tape"
}
test_mark_unmark


// Tests multiple, nested marking and unmarkings.
proc test_multiple_mark_unmark {
    // Simulates performing a computation by moving to the start of the tape,
    // and then back to the marking symbol.
    proc computation t:Tape mark:Sym {
        to_start t
        right_until t mark
    }

    let ms = Marks "" ""
    init_marks ms "1234"

    let t = "abcd"
    right t

    let mark1 = read (ms.free)
    mark t ms
    computation t mark1

    let mark2 = read (ms.free)
    right t
    mark t ms
    computation t mark2

    unmark t ms
    left t
    unmark t ms

    assert_tape_eq t "abcd" "multiple_mark_unmark_tape"
}
test_multiple_mark_unmark
