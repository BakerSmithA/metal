// Brainfuck langauge interpreter.
import tape
import int
import array_int
import ascii
import io

// effect : interprets the current command on the tape. Writes an output to out.
proc interpret_single instrs:Tape it:IntTape should_print:Sym out:Tape {
    let tok = read instrs

    /* unsafe_print_all "TOK: "
    print tok
    println */

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

        if should_print == '1' {
            print (read char)
        }

        write out (read char)
        right out

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
//          instructions remaining. Writes the output to out.
proc interpret instrs:Tape bit_len:Counter should_print:Sym out:Tape {
    let it = IntTape "" bit_len
    it_init it

    while read instrs != ' ' {
        interpret_single instrs it should_print out
        right instrs
    }
}

let out = ""
interpret main (Counter "12345678") '1' ""
