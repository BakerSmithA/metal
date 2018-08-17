import logic
import tape

// A binary integer where bits are ordered from least to most significant.
// e.g. to represent the number 2 in base 10, you would create the object:
//
//  > Int "01"
struct Int {
    // Binary representation of the number.
    bin:Tape
}

// Sets the value contained in num to the value contained in new.
// effect     : writes the contents of new to num.
// complexity : O(n), where n is the number of bits.
proc set num:Int new:Int {
    copy_until new.bin ' ' num.bin
    to_start num.bin
    to_start new.bin
}

// Sets new to contain 0, but with the same number of bits as num.
// E.g.
// > let new = (Int "")
// > set0_same_len (Int "11") new
// Leaves new containing "00"
//
// effect     : sets new to contain 0, but with the same number of bits as num.
// complexity : O(n), where n is the number of bits.
proc set0_same_len num:Int new:Int {
    while read num.bin != ' ' {
        write new.bin '0'
        right num.bin
        right new.bin
    }
    to_start num.bin
    to_start new.bin
}

// Sets new to contain 1, but with the same number of bits as num.
// E.g.
// > let new = (Int "")
// > set0_same_len (Int "111") new
// Leaves new containing "100"
//
// effect     : sets new to contain 1, but with the same number of bits as num.
// complexity : O(n), where n is the number of bits.
proc set1_same_len num:Int one:Int {
    set0_same_len num one
    write one.bin '1'
}

// Checks whether num has the value 0.
// E.g. 0000 has the value zero, but 0010 does not.
// effect     : writes either '0' or '1' to r depending on whether num is zero or not.
// complexity : O(n)
proc is_zero num:Int r:Tape {
    // To start we'll say the num is zero. If we find a '1' this will change
    // to false.
    write r '1'
    while read num.bin != ' ' {
        if read num.bin == '1' {
            write r '0'
        }
        right num.bin
    }
    to_start num.bin
}

// Checks the value of x against y.
// effect     : checks whether x==y, ignoring leading zeros. The result, either
//              '0' or '1', is written to r. Also returns the read-write head
//              of both integers to the start position after.
// complexity : O(n)
//
//  > eq_int (Int "01") (Int "01") r
//  Writes '1' to r
//
//  > eq_int (Int "010") (Int "01") r
//  Writes '1' to r
//
//  > eq_int (Int "0") (Int "1") r
//  Writes '0' to r
proc int_eq x:Int y:Int r:Tape {
    write r '1'
    while read x.bin != ' ' and read y.bin != ' ' {
        if read x.bin != read y.bin {
            write r '0'
        }
        right x.bin
        right y.bin
    }

    // In case the operands are different lengths, check for leading zeros.
    if (read r == '1') {
        if (read x.bin == ' ') and (read y.bin != ' ') {
            // Got to the end of x, but not of y.
            is_zero y r
        } else if (read x.bin != ' ') and (read y.bin == ' ') {
            // Got to the end of y, but not of x.
            is_zero x r
        }
    }

    to_start x.bin
    to_start y.bin
}

// Convenience function for copying integers.
// effect     : copies the contents of in to out. The head of all x is also
//              placed back at the start.
// complexity : O(n)
proc copy_int in:Int out:Int {
    copy_until in.bin ' ' out.bin
    // Zero both so the tapes are ready to be used in the addition,
    // subtraction, etc.
    to_start in.bin
    to_start out.bin
}

// Convenience function for copying operands of binary operations.
// effect     : copies the contents of x to cx and y to cy. The head of all
//              integers is also placed back at the start.
// complexity : O(n)
proc copy_operands x:Int y:Int cx:Int cy:Int {
    copy_int x cx
    copy_int y cy
}

// Convenience function for moving the read write head to the start.
// effect     : moves the read-write head of x, y, and r to the start.
// complexity : O(n)
proc zero_operands x:Int y:Int r:Int {
    to_start x.bin
    to_start y.bin
    to_start r.bin
}

// effect     : moves the read-write head of both x and y one position to the right.
// complexity : O(1)
proc right_operands x:Int y:Int {
    right x.bin
    right y.bin
}

// Computes r=x+y
// effect     : writes the binary representation x+y to r.
// complexity : O(n), where n is the number of bits.
proc add x:Int y:Int r:Int {
    proc r_add x:Int y:Int c_in:Sym r:Int {
        let p = read x.bin
        let q = read y.bin

        if p != ' ' and q != ' ' {
            bin_full_adder p q c_in r.bin
            // Read the carry bit from the tape to be used the next time add is called.
            let new_cin = read r.bin
            right_operands x y
            r_add x y new_cin r
        }
    }

    // Copying allows the same object to be used for both operands.
    let cx = Int ""
    let cy = Int ""
    copy_operands x y cx cy

    r_add cx cy '0' r
    // To allow r to be used with other operations later.
    to_start r.bin
}

// Computes r=x-y
// effect     : writes the binary representation of x-y to r.
// complexity : O(n), where n is the number of bits.
proc sub x:Int y:Int r:Int {
    proc r_sub x:Int y:Int borr_in:Sym r:Int {
        let p = read x.bin
        let q = read y.bin

        if p != ' ' and q != ' ' {
            bin_full_sub p q borr_in r.bin
            // Read the carry bit from the tape to be used the next time add is called.
            let new_borr_in = read r.bin
            right_operands x y
            r_sub x y new_borr_in r
        }
    }

    // Copying allows the same object to be used for both operands.
    let cx = Int ""
    let cy = Int ""
    copy_operands x y cx cy

    r_sub cx cy '0' r
    // To allow r to be used with other operations later.
    to_start r.bin
}

// Computes x+=dx
// effect     : writes the binary representation of x+dx to x.
// complexity : O(n)
proc inc x:Int dx:Int {
    add x dx x
}

// Computes x-=dx
// effect     : writes the binary representation of x-dx to x.
// complexity : O(n)
proc dec x:Int dx:Int {
    sub x dx x
}

/* proc append_0 x:Int {
    right_until x.bin ' '
    write x.bin '0'
    to_start x.bin
} */

// Computes r=x*y
// effect:    : writes the binary representation of x*y to r.
// complexity : O(n^2) where n is the number of bits.
proc mult x:Int y:Int r:Int {
    // Copying allows the same object to be used for both operands.
    let cx = Int ""
    let cy = Int ""
    copy_operands x y cx cy

    // Used to count y down to zero.
    let count = Int ""
    copy_int y count

    // Used to store true or false to indicate whether y == 0
    let is_z = ""
    is_zero count is_z

    let one = Int ""
    set1_same_len y one

    set0_same_len x r
    // Increment r by x, y times
    while (read is_z) == '0' {
        dec count one
        inc r x
        is_zero count is_z
    }
}

let r = Int ""
mult (Int "11000") (Int "111000") r
_print r.bin
