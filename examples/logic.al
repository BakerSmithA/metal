// Binary NOT gate.
// effect     : computes (not p) and writes the result to the tape t.
// complexity : O(1)
func bin_not p:Sym t:Tape {
    if p == '1' {
        write t '0'
    } else {
        write t '1'
    }
}

// Binary OR gate.
// effect     : computes (p or q) and writes the result to the tape t.
// complexity : O(1)
func bin_or p:Sym q:Sym t:Tape {
    if p == '1' or q == '1' {
        write t '1'
    } else {
        write t '0'
    }
}

// Binary AND gate.
// effect     : computes (p and q) and writes the result to the tape t.
// complexity : O(1)
func bin_and p:Sym q:Sym t:Tape {
    if p == '1' and q == '1' {
        write t '1'
    } else {
        write t '0'
    }
}

// Binary XOR gate.
// effect     : computes (p xor q) and writes the result to tape t.
// complexity : O(1)
func bin_xor p:Sym q:Sym t:Tape {
    // Note that 'p XOR q' is equivalent to '(p OR q) AND (NOT(p AND q))'.
    // a = p OR q
    bin_or p q t
    let a = read t

    // b = p AND q
    bin_and p q t
    let b = read t

    // c = NOT b
    bin_not b t
    let c = read t

    // d = a AND c
    bin_and a c t
}

// Binary half adder, which performs binary addition of two 1 bit operands.
// writes t+0 : the sum bit of p + q.
// writes t+1 : the carry bit of p + q
// complexity : O(1)
// ref: http  ://www.electronics-tutorials.ws/combination/comb_7.html
func bin_half_adder p:Sym q:Sym t:Tape {
    // The sum bit is computed as 'a XOR b'.
    bin_xor p q t

    // The carry bit is computed as 'a AND b'.
    right t
    bin_and p q t
}

// Full binary adder, which performs binary addition of three 1 bit operands.
// i.e. the two sum bits and a carry in bit.
// writes t+0 : the sum bit of the addition.
// writes t+1 : the carry-out bit of the addition.
// complexity : O(1)
// ref        : http://www.electronics-tutorials.ws/combination/comb_7.html
func bin_full_adder p:Sym q:Sym carry_in:Sym t:Tape {
    bin_half_adder p q t
    // ..|sum0|*carry0|..
    let carry0 = read t
    left t
    let sum0 = read t

    bin_half_adder sum0 carry_in t
    // ..|sum1|*carry1|..

    // The final carry is computed as the OR of the carry-out from both half
    // adders.
    bin_or (read t) carry0 t
}
