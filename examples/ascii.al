import int

let ascii = "!'#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

// effect     : writes the ascii symbol at the given index to the tape r.
// complexity : O(n)
proc int_to_ascii i:Int r:Tape {
    let ci = Int ""
    copy_int i ci

    // Decrement i until zero is reached, i.e. the location in the array.
    let is_z = "0"
    while read is_z == '0' {
        
    }
}
