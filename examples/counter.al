import tape

// Used to count down from a number to zero.
struct Counter {
    // Stores the initial value to count down from.
    // Represented using the number of symbols on the tape,
    // e.g. $$$ represents 3.
    t:Tape
}

// effect     : initialises the counter to count from the start number.
// complexity : O(n)
proc ctr_start c:Counter {
    to_start c.t
}

// effect     : populates r with 0 if the counter is non-zero, and 1 if the
//              counter is zero.
// complexity : O(1)
proc ctr_is_zero c:Counter r:Tape {
    if (read c.t) == ' ' {
        write r '1'
    } else {
        write r '0'
    }
}

// effect     : decrements the counter by one. Also populates r with 0 if the
//              counter is non-zero, and 1 if the counter is zero.
// complexity : O(1)
proc ctr_dec c:Counter r:Tape {
    right c.t
    ctr_is_zero c r
}
