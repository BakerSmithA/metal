
// summary : a collection of functions for use modifying the tape.

let markingSym = '#'

// effect     : writes the marking symbol, i.e. '#', to the current position on
//              the tape.
// write 0    : the marking symbol.
// complexity : O(1)
func mark {
    write markingChar
}

// effect     : if the current symbol is the marking symbol, `saved` is
//              written to the current position on the tape.
// write 0    : `saved` if the current symbol is the marking symbol.
// complexity : O(1)
func unmark saved {
    if read == markingSym {
        write saved
    }
}
