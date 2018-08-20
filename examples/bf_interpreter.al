// Brainfuck langauge interpreter.
import tape
import int
import array_int
import ascii

// effect : interprets the current command on the tape.
proc interpret_single instrs:Tape it:IntTape {
    let tok = read instrs

    if tok == '>' {
        it_right it

    } else if tok == '<' {
        it_left it

    } else if tok == '+' {
        let r = Int ""
        it_read it r
        inc_by_1 r
        it_write it r

    } else if tok == '-' {
        let r = Int ""
        it_read it r
        dec_by_1 r
        it_write it r

    } else if tok == '.' {
        let x = Int ""
        it_read it x

        let char = ""
        int_to_ascii x char

        print (read char)

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
proc interpret instrs:Tape bit_len:Counter {
    let it = IntTape "" bit_len
    it_init it

    while read instrs != ' ' {
        interpret_single instrs it
        right instrs

        _print it.t
    }

    /* _print it.t */
}

interpret main (Counter "12345678")
