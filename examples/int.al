import logic

struct Int {
    // Binary representation of the number.
    bin:Tape
}

// Computes x+y=out
// effect: writes the binary representation x+y to out.
// complexity: O(n), where n is the number of bits.
func add x:Int y:Int out:Int {
    // Used as first carry in bit.
    write out.bin '0'

    while read x.bin != ' ' and read y.bin != ' ' {
        let p = read x.bin
        let q = read y.bin
        let c_in = read out.bin

        bin_full_adder p q c_in out.bin

        right x.bin
        right y.bin
        right out.bin
    }
}

let x = Int "11"
let y = Int "01"
let z = Int ""

add x y z

_print z.bin
