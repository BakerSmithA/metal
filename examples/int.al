import logic
import zero
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
func set num:Int new:Int {
    copy_until new.bin ' ' num.bin
    zero num.bin
    zero new.bin
}

// Checks whether num has the value 0.
// E.g. 0000 has the value zero, but 0010 does not.
// effect     : writes either '0' or '1' to r depending on whether num is zero or not.
// complexity : O(n)
func is_zero num:Int r:Tape {
    // To start we'll say the num is zero. If we find a '1' this will change
    // to false.
    write r '1'
    while read num.bin != ' ' {
        if read num.bin == '1' {
            write r '0'
        }
        right num.bin
    }
    zero num.bin
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
func int_eq x:Int y:Int r:Tape {
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

    zero x.bin
    zero y.bin
}

// Convenience function for copying operands of binary operations.
// effect     : copies the contents of x to cx and y to cy. The head of all
//              integers is also placed back at the start.
// complexity : O(n)
func copy_operands x:Int y:Int cx:Int cy:Int {
    func copy_int in:Int out:Int {
        copy_until in.bin ' ' out.bin
        // Zero both so the tapes are ready to be used in the addition,
        // subtraction, etc.
        zero in.bin
        zero out.bin
    }

    copy_int x cx
    copy_int y cy
}

// Convenience function for moving the read write head to the start.
// effect     : moves the read-write head of x, y, and r to the start.
// complexity : O(n)
func zero_operands x:Int y:Int r:Int {
    zero x.bin
    zero y.bin
    zero r.bin
}

// effect     : moves the read-write head of both x and y one position to the right.
// complexity : O(1)
func right_operands x:Int y:Int {
    right x.bin
    right y.bin
}

// Computes r=x+y
// effect     : writes the binary representation x+y to r.
// complexity : O(n), where n is the number of bits.
func add x:Int y:Int r:Int {
    func r_add x:Int y:Int c_in:Sym r:Int {
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
    zero r.bin
}

// Computes r=x-y
// effect     : writes the binary representation of x-y to r.
// complexity : O(n), where n is the number of bits.
func sub x:Int y:Int r:Int {
    func r_sub x:Int y:Int borr_in:Sym r:Int {
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
    zero r.bin
}

// Computes x+=dx
// effect     : writes the binary representation of x+dx to x.
// complexity : O(n)
func inc x:Int dx:Int {
    add x dx x
}

// Computes x-=dx
// effect     : writes the binary representation of x-dx to x.
// complexity : O(n)
func dec x:Int dx:Int {
    sub x dx x
}
