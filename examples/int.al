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

struct Int {
    // Binary representation of the number.
    bin:Tape
}

// Computes r=x+y
// effect     : writes the binary representation x+y to out.
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

/* func add x:Int y:Int out:Int {
    // Copying allows both operands to be the same tape, and stops modification
    // of the original tape.
    let cx = Int ""
    let cy = Int ""
    copy x.bin cx.bin
    copy y.bin cy.bin

    // Used as first carry in bit.
    write out.bin '0'

    while read cx.bin != ' ' and read cy.bin != ' ' {
        let p = read cx.bin
        let q = read cy.bin
        let c_in = read out.bin

        bin_full_adder p q c_in out.bin

        right cx.bin
        right cy.bin
        right out.bin
    }
} */
