// Binary NOT gate.
// effect     : computes (not p) and writes the result to the tape t.
// complexity : O(1)
proc bin_not p:Sym t:Tape {
    if p == '1' {
        write t '0'
    } else {
        write t '1'
    }
}

// Binary OR gate.
// effect     : computes (p or q) and writes the result to the tape t.
// complexity : O(1)
proc bin_or p:Sym q:Sym t:Tape {
    if p == '1' or q == '1' {
        write t '1'
    } else {
        write t '0'
    }
}

// Binary AND gate.
// effect     : computes (p and q) and writes the result to the tape t.
// complexity : O(1)
proc bin_and p:Sym q:Sym t:Tape {
    if p == '1' and q == '1' {
        write t '1'
    } else {
        write t '0'
    }
}

// Binary XOR gate.
// effect     : computes (p xor q) and writes the result to tape t.
// complexity : O(1)
proc bin_xor p:Sym q:Sym t:Tape {
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
proc bin_half_adder p:Sym q:Sym t:Tape {
    // Sum bit = p xor q
    bin_xor p q t

    // Carry bit = p and q
    right t
    bin_and p q t
}

// Full binary adder, which performs binary addition of three 1 bit operands.
// i.e. the two sum bits and a carry in bit.
// writes t+0 : the sum bit of the addition.
// writes t+1 : the carry-out bit of the addition.
// complexity : O(1)
// ref        : http://www.electronics-tutorials.ws/combination/comb_7.html
proc bin_full_adder p:Sym q:Sym carry_in:Sym t:Tape {
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

// Binary half subractor, which performs binary subtraction of two 1 bit operands.
// writes t+0 : the difference bit of p - q.
// writes t+1 : the borrow bit of p - q.
// complexity : O(1)
// ref        : https://www.electronics-tutorials.ws/combination/binary-subtractor.html
proc bin_half_sub p:Sym q:Sym t:Tape {
    // Different bit = p xor q
    bin_xor p q t

    // Borrow bit = (not p) and q
    right t
    bin_not p t
    bin_and (read t) q t
}

// Binary full subtractor, which performs binary subtraction of three 1 bit operands.
// writes t+0 : the difference bit of the subtraction.
// writes t+1 : the borrow bit of the subtraction.
// complexity : O(1)
// ref        : https://www.electronics-tutorials.ws/combination/binary-subtractor.html
proc bin_full_sub p:Sym q:Sym borrow_in:Sym t:Tape {
    bin_half_sub p q t
    // ..|diff0|*borrow0|..
    let borrow0 = read t
    left t
    let diff0 = read t

    bin_half_sub diff0 borrow_in t
    // ..|diff1|*borrow1|...

    // The final borrow is computed as the OR of the borrow out from both
    // half subtractors.
    bin_or (read t) borrow0 t
}
