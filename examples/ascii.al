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
