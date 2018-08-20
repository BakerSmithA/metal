import lexer
import ../test

// Tests consuming input generates the correct token as output.
proc test_consume_makes_token {
    proc assert_consume input:Tape expected_type:Sym expected_val:Tape name:Tape {
        let type_name = "token_type_"
        let val_name = "token_val_"

        append type_name name
        append val_name name

        let actual = Tok "" ""
        consume input actual

        let expected = Tok "" expected_val
        set_tok_type expected expected_type

        assert_tape_eq actual.type expected.type type_name
        assert_tape_eq actual.val expected.val val_name
    }

    assert_consume "+" plus_tok_type "" "plus"
    assert_consume "-" minus_tok_type "" "minus"
    assert_consume "1234" num_tok_type "1234" "number"
    assert_consume "#" eof_tok_type "" "eof"
    assert_consume "   +" plus_tok_type "" "skips_whitespace"

}
test_consume_makes_token

// Tests consuming input moves the read-write head of the input.
/* proc test_consume_moves_head {

}
test_consume_moves_head */
