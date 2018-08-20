import ../ascii
import ../tape
import ../io

let whitespace_tok_type = ' '
let bin_op_tok_type = '_'
let num_tok_type = 'n'
let eof_tok_type = '#'

struct Tok {
    type:Tape
    val:Tape
}

proc print_tok tok:Tok {
    unsafe_print_all "Type:"
    unsafe_print_all tok.type
    println
    to_start tok.type

    if read tok.val != ' ' {
        unsafe_print_all "Val:"
        unsafe_print_all tok.val
        println
        to_start tok.val
    }
}

// effect     : sets the type of the token, e.g. whitespace.
// complexity : O(1)
proc set_tok_type tok:Tok type:Sym {
    write tok.type type
}

// effect     : attemps to parse a number by keeping taking digits until none are left.
// complexity : O(n)
proc try_consume_num input:Tape tok:Tok {
    let is_digit = ""
    ascii_is_digit (read input) is_digit

    if read is_digit == '1' {
        set_tok_type tok num_tok_type

        while read is_digit == '1' {
            write tok.val (read input)
            right tok.val
            right input
            ascii_is_digit (read input) is_digit
        }

        to_start tok.val
        
    } else {
        right input
    }
}

// effect      : parses the next token on the input. Populates tok with the read token.
// param input : input characters to tokenise.
// param tok   : populated with the next token on the input.
// complexity  : O(1)
proc consume input:Tape tok:Tok {
    set_tok_type tok whitespace_tok_type

    while read tok.type == whitespace_tok_type {
        let next_char = read input

        if next_char == ' ' {
            set_tok_type tok whitespace_tok_type

        } else if next_char == '+' or next_char == '-' {
            set_tok_type tok bin_op_tok_type
            write tok.val next_char

        } else if next_char == '#' {
            set_tok_type tok eof_tok_type

        } else {
            try_consume_num input tok
        }
    }
}
