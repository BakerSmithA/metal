// effect     : copies the contents of in to out until a terminating character is reached.
// warning    : copying begins from the current position on each tape.
// warning    : leaves the head at the point where the first term is read.
// complexity : O(n)
proc copy_until in:Tape term:Sym out:Tape {
    while read in != term {
        write out (read in)
        right in
        right out
    }
}

// effect     : moves the read-write head to the start of the tape.
// warning    : uses '#' as a marking symbol.
// complexity : O(n)
proc to_start t:Tape {
    let at_start = "0"

    while read at_start == '0' {
        let saved = read t
        // Mark the current position of the head.
        write t '#'

        // Try to move left. If we are at the start then this will have no effect.
        left t

        if read t == '#' {
            // The head did not move, therefore we're at the start.
            // This is the base case of the function.
            write t saved

            // Break out of the loop.
            write at_start '1'

        } else {
            // The head did move, therefore we are not at the start.
            // We need to replace the overwritten symbol with the original and
            // then continue searching.
            right t
            write t saved
            left t
            left t
        }
    }
}

// effect     : moves the read-write head right until the term is encountered.
// complexity : O(n)
proc right_until t:Tape term:Sym {
    while read t != term {
        right t
    }
}

// effect     : moves the read-write head left until the term is encountered.
// complexity : O(n)
proc left_until t:Tape term:Sym {
    while read t != term {
        left t
    }
}


// effect     : appends end onto the end of start.
// warning    : leaves the read-write of both tapes at their starts.
// complexity : O(n)
proc append start:Tape end:Tape {
    right_until start ' '
    to_start end

    while read end != ' ' {
        write start (read end)
        right start
        right end
    }

    to_start start
    to_start end
}
