import ascii
import test

proc test_int_to_ascii {
    proc assert_ascii i:Int exp:Sym name:Tape {
        let r = ""
        int_to_ascii i r

        assert_sym_eq (read r) exp name
    }

    assert_ascii (Int "1000001") 'A' "A"
    assert_ascii (Int "1000011") 'a' "a"
    assert_ascii (Int "0001110") '8' "8"
}
test_int_to_ascii
