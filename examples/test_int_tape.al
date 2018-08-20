import int_tape
import test

// effect : tests that actual and expected are equal. If not then the message
//          will be printed.
proc assert_int_eq actual:Int expected:Int name:Tape {
    let r = ""
    int_eq actual expected r
    assert_tape actual.bin expected.bin r name
}

// effect : tests the the contents of the int tape is expected.
proc assert_it_eq actual:IntTape expected:Tape name:Tape {
    assert_tape_eq actual.t expected name
}

// Tests initialising sets the first cell to contain 0.
proc test_init {
    let it = IntTape "" (Counter "12345")
    it_init it

    assert_it_eq it "00000" "init"
}
test_init

// Tests moving right adds a new cell, if there is not one there.
proc test_right_creates_new {
    let it = IntTape "" (Counter "12345")
    it_init it

    it_right it

    assert_it_eq it "0000000000" "right_creates_new"
}
test_right_creates_new

// Tests moving right does not overwrite an already existing cell.
proc test_right_does_not_overwrite {
    let it = IntTape "" (Counter "12345")
    it_init it

    it_right it
    it_left it
    it_write it (Int "11111")
    it_right it

    assert_it_eq it "1111100000" "right_does_not_overwrite"
}
test_right_does_not_overwrite

// Tests writing to the tape.
proc test_write {
    let it = IntTape "" (Counter "12345")
    it_init it

    it_write it (Int "10101")

    assert_it_eq it "10101" "write"
}
test_write

// Tests reading from the tape.
proc test_read {
    let it = IntTape "" (Counter "12345")
    it_init it

    it_write it (Int "10101")

    let r = Int ""
    it_read it r

    assert_int_eq r (Int "10101") "read"
}
test_read
