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
