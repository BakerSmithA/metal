import logic

struct Int {
    // Binary representation of the number.
    bin:Tape
}

func zero t:Tape {
    let saved = read t
    // Mark the current position of the head.
    write t '#'

    // Try to move left. If we are at the start then this will have no effect.
    left t

    if read t == '#' {
        // The head did not move, therefore we're at the start.
        // This is the base case of the function.
        write t saved
    } else {
        // The head did move, therefore we are not at the start.
        // We need to replace the overwritten symbol with the original and
        // then continue searching.
        right t
        write t saved
        left t
        left t

        // Recursive call.
        zero t
    }
}

func copy in:Tape out:Tape {
    while read in != ' ' {
        write out (read in)
        right in
        right out
    }

    zero in
    zero out
}

// Computes out=x+y
// effect     : writes the binary representation x+y to out.
// complexity : O(n), where n is the number of bits.
func add x:Int y:Int out:Int {
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
}

// Computes out=x-y
// effect     : writes the binary representation x-y to out.
// complexity : O(n), where n is the number of bits.
func sub x:Int y:Int out:Int {
    // Copying allows both operands to be the same tape, and stops modification
    // of the original tape.
    let cx = Int ""
    let cy = Int ""
    copy x.bin cx.bin
    copy y.bin cy.bin

    // Used as first borrow in bit.
    write out.bin '0'

    while read cx.bin != ' ' and read cy.bin != ' ' {
        let p = read cx.bin
        let q = read cy.bin
        let b_in = read out.bin

        bin_full_sub p q b_in out.bin

        right cx.bin
        right cy.bin
        right out.bin
    }
}

let x = Int "11"
let y = Int "10"
let z = Int ""

sub x y z

_print x.bin
_print y.bin
_print z.bin
