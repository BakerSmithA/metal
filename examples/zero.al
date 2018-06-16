func zero t:Tape {
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
        zero t
    }
}
