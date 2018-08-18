import counter
import int

// Tape containing integers.
struct IntTape {
    // Stores the integers.
    t:Tape
    // The number of bits in each integer.
    bit_len:Counter
}

// effect     : moves the read-write head either left (if sym is 'l') or right
//              if sym is 'r').
// complexity : O(1)
proc it_move it:IntTape dir:Sym {
    ctr_start it.bit_len

    let is_z = ""
    ctr_is_zero it.bit_len is_z

    while (read is_z) == '0' {
        if (dir == 'l') {
            left it.t
        } else if (dir == 'r') {
            right it.t
        }
        ctr_dec it.bit_len
    }
}

// effect     : moves the read-write head one cell to the left. Or performs no
//              action if the head is already at the start.
// complexity : O(1)
proc it_left it:IntTape {
    it_move it 'l'
}

// effect     : writes an integer containing 0 to the current position on the tape.
// warning    : modifies the read-write head position to be the end of the written int.
// complexity : O(1)
proc it_write_0 it:IntTape {
    ctr_start it.bit_len

    let is_z = ""
    ctr_is_zero it.bit_len is_z

    while (read is_z) == '0' {
        right it.t
        write it.t '0'
        ctr_dec it.bit_len
    }
}


// effect     : moves the read-write head one cell to the right. If the new
//              position is empty then a new integer is added to the array.
// complexity : O(1)
proc it_right it:IntTape {
    it_move it 'r'

    if (read it.t) == ' ' {
        it_write_0 it
        it_left it
    }
}

// effect     : writes the given integer to the current position on the tape.
// complexity : O(1)
proc it_write it:IntTape num:Int {
    copy_int_cnts num it.t
}

// effect     : reads the integer at the current position and writes it to r.
// complexity : O(1)
proc it_read it:IntTape r:Int {
    
}
