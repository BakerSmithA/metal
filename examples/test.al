// effect: writes either '0' or '1' to out depending on whether t1 and t2 are equal.
func eq_tape t1:Tape t2:Tape out:Tape {
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
func assert_tape actual:Tape expected:Tape result:Tape name:Tape {
    if read result == '1' {
        _print "Passed: "
        _print name

    } else {
        _print "Failed: "
        _print name
        _print "\n\tExpect:\t"
        _print expected
        _print "\n\tGot:\t"
        _print actual
    }
}

func zero t:Tape {
    let saved = read t
    // Mark the current position of the head.
    write t '#'

    // Try to move left. If we are at the start then this will have no effect.
    left t

    if read t == '#' {
        // The head did not move, therefore we're at the start.
        // This is the base case of the function.
        write t saved
    } else {
        // The head did move, therefore we are not at the start.
        // We need to replace the overwritten symbol with the original and
        // then continue searching.
        right t
        write t saved
        left t
        left t

        // Recursive call.
        zero t
    }
}

// effect: tests that actual and expected are equal. If not then the message
//      will be printed.
func assert_tape_eq actual:Tape expected:Tape name:Tape {
    zero actual
    zero expected

    let result = ""
    eq_tape actual expected result
    assert_tape actual expected result name
}
