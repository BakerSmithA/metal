import tape

// effect     : prints the contents of the tape until a ' ' is encountered.
//              Prints from the current position of the read write head.
// warning    : modifies the position of the read-write head.
// complexity : O(n)
proc unsafe_print_all t:Tape {
    while (read t) != ' ' {
        print (read t)
        right t
    }
}

// effect     : prints fthe contents of the tape from the start until a ' '
//              is encountered.
// warning    : modifies the position of the read-write head.
// complexity : O(n)
proc unsafe_print_all_start t:Tape {
    to_start t
    unsafe_print_all t
}

// effect     : prints out the contents of the tape and returns position to
//              original location.
// warning    : uses $ as a marking symbol.
// complexity : O(n)
proc print_all t:Tape {
    let mark = '$'

    let saved = read t
    write t mark

    to_start t

    while read t != ' ' {
        if read t == mark {
            print saved
        } else {
            print (read t)
        }
        right t
    }

    left_until t mark
    write t saved
}
