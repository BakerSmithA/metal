import logic
import zero

func copy in:Tape out:Tape {
    while read in != ' ' {
        write out (read in)
        right in
        right out
    }

    zero in
    zero out
}

// A binary integer where bits are ordered from least to most significant.
// e.g. to represent the number 2 in base 10, you would create the object:
//
//  > Int "01"
struct Int {
    // Binary representation of the number.
    bin:Tape
}

// Computes r=x+y
// effect     : writes the little endian binary representation x+y to r.
// complexity : O(n), where n is the number of bits.
func add x:Int y:Int r:Int {
    func r_add x:Int y:Int c_in:Sym r:Int {
        let p = read x.bin
        let q = read y.bin

        if p != ' ' and q != ' ' {
            bin_full_adder p q c_in r.bin
            // Read the carry bit from the tape to be used the next time add is called.
            let new_cin = read r.bin
            right x.bin
            right y.bin
            r_add x y new_cin r
        }
    }

    r_add x y '0' r
}

// Computes r=x-y
// effect     : writes the little endian binary representation of x-y to r.
// complexity : O(n), where n is the number of bits.
func sub x:Int y:Int r:Int {
    func r_sub x:Int y:Int b_in:Sym r:Int {
        let p = read x.bin
        let q = read y.bin

        if p != ' ' and q != ' ' {
            bin_full_sub p q b_in r.bin
            // Read the carry bit from the tape to be used the next time add is called.
            let new_bin = read r.bin
            right x.bin
            right y.bin
            r_sub x y new_bin r
        }
    }

    r_sub x y '0' r
}
