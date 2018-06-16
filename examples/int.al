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

// effect     : writes either '0' or '1' to r depending on whether num is zero or not.
// complexity : O(n)
func is_zero num:Int r:Tape {
    write r '1'
    while read num.bin != ' ' {
        if read num.bin == '1' {
            write r '0'
        }
        right num.bin
    }
    zero num.bin
}

// Computes r=x+y
// effect     : writes the binary representation x+y to r.
// complexity : O(n), where n is the number of bits.
func add x:Int y:Int r:Int {
    func r_add x:Int y:Int c_in:Sym r:Int {
        let p = read x.bin
        let q = read y.bin

        // OR allows operands of different lengths to be added.
        if p != ' ' or q != ' ' {
            bin_full_adder p q c_in r.bin
            // Read the carry bit from the tape to be used the next time add is called.
            let new_cin = read r.bin
            right x.bin
            right y.bin
            r_add x y new_cin r
        }
    }

    // Copying allows the same object to be used for both operands.
    let cx = Int ""
    let cy = Int ""
    copy x.bin cx.bin
    copy y.bin cy.bin

    r_add cx cy '0' r
}

// Computes r=x-y
// effect     : writes the binary representation of x-y to r.
// complexity : O(n), where n is the number of bits.
func sub x:Int y:Int r:Int {
    func r_sub x:Int y:Int b_in:Sym r:Int {
        let p = read x.bin
        let q = read y.bin

        // OR allows operands of different lengths to be added.
        if p != ' ' or q != ' ' {
            bin_full_sub p q b_in r.bin
            // Read the carry bit from the tape to be used the next time add is called.
            let new_bin = read r.bin
            right x.bin
            right y.bin
            r_sub x y new_bin r
        }
    }

    // Copying allows the same object to be used for both operands.
    let cx = Int ""
    let cy = Int ""
    copy x.bin cx.bin
    copy y.bin cy.bin

    r_sub cx cy '0' r
}

// Computes r=x*y
// effect     : writes the binary representation of x*y to r.
// complexity : O(n²), where n is the number of bits.
func mult x:Int y:Int r:Int {
    let end = "0"
    while read end == '0' {
        is_zero y end
    }
}
