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
    zero r.bin
}
