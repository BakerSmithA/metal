import tape
import io

// effect: writes either '0' or '1' to out depending on whether t1 and t2 are equal.
proc eq_tape t1:Tape t2:Tape out:Tape {
    write out '1'

    while ((read t1 != ' ') or (read t2 != ' ')) and (read out == '1') {
        if read t1 != read t2 {
            write out '0'
        }
        right t1
        right t2
    }
}

// effect: prints that the test passed or failed.
proc assert_tape actual:Tape expected:Tape result:Tape name:Tape {
    if read result == '1' {
        unsafe_print_all "Passed:"
        unsafe_print_all name
        println

    } else {
        println
        unsafe_print_all "Failed:"
        unsafe_print_all name
        println
        unsafe_print_all "Expect:"
        unsafe_print_all_start expected
        println
        unsafe_print_all "Got:"
        unsafe_print_all_start actual
        println
        println
    }
}

// effect: tests that actual and expected are equal. If not then the message
//      will be printed.
proc assert_tape_eq actual:Tape expected:Tape name:Tape {
    to_start actual
    to_start expected

    let result = ""
    eq_tape actual expected result
    assert_tape actual expected result name
}

// effect: tests that the two symbols are equal.
proc assert_sym_eq actual:Sym expected:Sym name:Tape {
    let actual_t = ""
    write actual_t actual

    let expected_t = ""
    write expected_t expected

    assert_tape_eq actual_t expected_t name
}
