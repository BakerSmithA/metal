import lexer


// effect : prints an error message to the screen.
proc err_expected expected:Tape actual:Tok {
    print_all "Expected:"
    print_all expected
    println
    print_all "Got:"
    println
    print_tok actual
}


proc term input:Tape {
    let tok = Tok "" ""
    consume input tok

    if read tok.type == num_tok_type {
        print_all tok.val
        to_start tok.val

    } else {
        err_expected "integer" tok
    }
}

proc expr input:Tape {
    term input

    let should_parse = "1"
    while read should_parse == '1' {
        let tok = Tok "" ""
        consume input tok

        if read tok.type == bin_op_tok_type {
            right input
            term input

            print (read tok.val)

        } else if read tok.type == eof_tok_type {
            write should_parse '0'

        } else {
            err_expected "binop" tok
            write should_parse '0'
        }
    }
}

expr main
