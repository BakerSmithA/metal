import tape

// Used to tell where the start of the tape is.
let _start_marker = 'S'

// Maintains where the start of the tape is, allowing the read-write head to
// be move to the start of the tape more efficiently than the normal method of
// writing a marker and checking whether the head moved.
struct ZTape {
    t:Tape
}

// effect     : adds a marker to keeps track of where the start of the tape is.
//              This allows moving to the start of the tape to be more efficient.
// warning    : adds a marker to the current position of the head, which is
//              assumed to be the start of the tape.
// complexity : O(n)
func zinit z:ZTape contents:Tape {
    copy_until contents ' ' z.t
}

// effect     : moves the read-write head to the start of the tape, i.e. the
//              position to the right of the start marker.
// warning    : assumes the tape has been initialised using `ztape` with a
//              marker at the start.
// complexity : O(n)
func to_zstart z:ZTape {
    while read z.t != _start_marker {
        left z.t
    }
    // So the head is not over the start marker.
    right z.t
}

func zwrite z:ZTape s:Sym {
    write z.t s
}

func zleft z:ZTape {
    left z.t
}

func zright z:ZTape {
    right z.t
}
