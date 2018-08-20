import io
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

// effect     : prints the binary representation of the integer.
// complexity : O(n)
proc int_print i:Int {
    unsafe_print_all i.bin
    to_start i.bin
}

// param binary : binary integer where bits are ordered from least to most
//                significant. E.g. to represent the number 2 in base 10,
//                you would use "01".
// effect       : copies the binary integer in t into the integer.
// warning      : modifies the position of the read-write head of `binary`.
// complexity   : O(n)
proc init_int binary:Tape r:Int {
    copy_until binary ' ' r.bin
    to_start r.bin
}

// effect     : copies the contents (binary representation) of the integer to
//              the given tape t.
// complexity : O(n)
proc copy_int_cnts i:Int t:Tape {
    copy_until i.bin ' ' t
    to_start i.bin
}

// Convenience function for copying integers.
// effect     : copies the contents of in to out. The head of all x is also
//              placed back at the start.
// complexity : O(n)
proc copy_int in:Int out:Int {
    copy_int_cnts in out.bin
    to_start out.bin
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
// warning    : The length of r is the same as that of x and y. Therefore, the
//              user of the function must ensure there are enough bits to store
//              the result.
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
    // Erase last bit to keep the output the same number of bits as input.
    write r.bin ' '
    // To allow r to be used with other operations later.
    to_start r.bin
}

// Computes r=x-y
// effect     : writes the binary representation of x-y to r.
// warning    : The length of r is the same as that of x and y. Therefore, the
//              user of the function must ensure there are enough bits to store
//              the result.
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
    // Erase last bit to keep the output the same number of bits as input.
    write r.bin ' '
    // To allow r to be used with other operations later.
    to_start r.bin
}

// Computes x+=dx
// effect     : writes the binary representation of x+dx to x.
// warning    : The length of r is the same as that of x. Therefore, the
//              user of the function must ensure there are enough bits to store
//              the result.
// complexity : O(n)
proc inc x:Int dx:Int {
    add x dx x
}

// Computes x+=1
// effect     : writes the binary representation of x+1 to x
// complexity : O(n)
proc inc_by_1 x:Int {
    let one = Int ""
    set1_same_len x one

    inc x one
}

// Computes x-=dx
// effect     : writes the binary representation of x-dx to x.
// warning    : The length of r is the same as that of x. Therefore, the
//              user of the function must ensure there are enough bits to store
//              the result.
// complexity : O(n)
proc dec x:Int dx:Int {
    sub x dx x
}

// Computes x-=1
// effect     : writes the binary representation of x-1 to x.
// complexity : O(n)
proc dec_by_1 x:Int {
    let one = Int ""
    set1_same_len x one

    dec x one
}

// Computes x-=1, and populates is_z with whether x is zero.
// effect     : writes x-=1 to x, and populates r with whether x is zero.
// complexity : O(n)
proc check_dec_by_1 x:Int is_z:Tape {
    dec_by_1 x
    is_zero x is_z
}

// Computes r=x*y
// effect:    : writes the binary representation of x*y to r.
// warning    : The length of r is the same as that of x and y. Therefore, the
//              user of the function must ensure there are enough bits to store
//              the result.
// complexity : O(n^2) where n is the number of bits.
proc mult x:Int y:Int r:Int {
    // Append 0 to the end of x. This gives the correct number of bits for
    // multiplication.
    let cx = Int ""
    copy_int x cx
    right_until cx.bin ' '
    write cx.bin '0'
    to_start cx.bin

    // Used to count y down to zero.
    let count = Int ""
    copy_int y count

    // Used to store true or false to indicate whether y == 0
    let is_z = ""
    is_zero count is_z

    let one = Int ""
    set1_same_len y one

    set0_same_len cx r
    // Increment r by x, y times
    while (read is_z) == '0' {
        check_dec_by_1 count is_z
        inc r cx
    }
}

// effect     : moves the read-write head right by the given number of steps.
// complexity : O(n)
proc right_by i:Int t:Tape {
    let ci = Int ""
    copy_int i ci

    // Decrement i until zero is reached, i.e. the location in the array.
    let is_z = ""
    is_zero ci is_z

    while read is_z == '0' {
        check_dec_by_1 ci is_z
        right t
    }
}
