import int

// effect     : writes the ascii symbol at the given index to the tape r.
// complexity : O(n)
proc int_to_ascii i:Int r:Tape {
    let ascii = "!_#$%&_()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
    right_by ascii i
    write r (read ascii)
}
