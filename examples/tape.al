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

// effect     : moves the read-write head to the start of the tape.
// complexity : O(n)
func to_start t:Tape {
    let saved = read t
    // Mark the current position of the head.
    write t '#'

    // Try to move left. If we are at the start then this will have no effect.
    left t

    if read t == '#' {
        // The head did not move, therefore we're at the start.
        // This is the base case of the function.
        write t saved
    } else {
        // The head did move, therefore we are not at the start.
        // We need to replace the overwritten symbol with the original and
        // then continue searching.
        right t
        write t saved
        left t
        left t

        // Recursive call.
        to_start t
    }
}
