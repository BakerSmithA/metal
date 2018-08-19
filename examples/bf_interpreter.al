// Brainfuck langauge interpreter.
import tape
import int
import array_int

// effect     : reads the current integer into r. Also creates a new int
//              with the value 1 which contains the same number of bits as
//              the read value.
// complexity : O(1)
proc read_one it:IntTape r:Int one:Int {
    it_read it r
    set1_same_len r one
}

// effect : interprets the current command on the tape.
proc interpret_single instrs:Tape it:IntTape {
    let tok = read instrs

    if tok == '>' {
        it_right it

    } else if tok == '<' {
        it_left it

    } else if tok == '+' {
        let r = Int ""
        let one = Int ""
        read_one it r one

        inc r one

        it_write it r

    } else if tok == '-' {
        let r = Int ""
        let one = Int ""
        read_one it r one

        dec r one

        it_write it r

    } else if tok == '.' {
        let r = Int ""
        it_read it r
        int_print r
        println

    } else if tok == '[' {
        // If the byte at the data pointer is zero, then jump forward to
        // the command after the matching ] command.
        let x = Int ""
        it_read it x

        let is_z = ""
        is_zero x is_z

        if read is_z == '1' {
            right_until instrs ']'
        }

    } else if tok == ']' {
        // If the byte at the data pointer is nonzero, then jump it back to the
        // command after the matching [ command.
        let x = Int ""
        it_read it x

        let is_z = ""
        is_zero x is_z

        if read is_z == '0' {
            left_until instrs '['
        }
    }
}

// effect : interprets the brainfuck instructions until there are no
//          instructions remaining.
proc interpret instrs:Tape {
    let it = IntTape "" (Counter "1234")
    it_init it

    while read instrs != ' ' {
        interpret_single instrs it
        right instrs
    }

    _print it.t
}

interpret main
