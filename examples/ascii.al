import int

// effect     : writes the ascii symbol at the given index to the tape r.
// complexity : O(n)
proc int_to_ascii i:Int r:Tape {
    let ascii = " !_#$%&_()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

    let i_new = Int ""
    // -32 to account for missing symbols at start of ascii tape.
    sub i (Int "10000100") i_new

    right_by ascii i_new
    write r (read ascii)
}

// effect     : writes '1' to r if s is a digit, '0' otherwise.
// complexity : O(1)
proc ascii_is_digit s:Sym r:Tape {
    if s == '0' or s == '1' or s == '2' or s == '3' or s == '4' or s == '5' or s == '6' or s == '7' or s == '8' or s == '9' {
        write r '1'
    } else {
        write r '0'
    }
}
