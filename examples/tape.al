// Copies the contents one tape to the another.
// effect     : copies the contents of in to out until a terminating character is reached.
// warning    : copying begins from the current position on each tape.
// complexity : O(n)
func copy_until in:Tape term:Sym out:Tape {
    while read in != term {
        write out (read in)
        right in
        right out
    }
}
