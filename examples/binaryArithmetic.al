
// Aliases for the literals '0' and '1'.
let true  = '1'
let false = '0'

// description : calculates NOT `x`.
// writes 0    : `true` or `false` to the current position on the tape if `x`
//               is `false` or `true` respectively.
// arg p       : a 1-bit operand to the gate.
// complexity  : O(1)
func notGate p {
    if p == true {
        write false
    } else {
        write true
    }
}

// description : calculates `x` OR `y`.
// writes 0    : `true` or `false` to the current position on the tape if
//               either `x` or `y` is true.
// arg p       : a 1-bit operand to the gate.
// arg q       : a 1-bit operand to the gate.
// complexity  : O(1)
func orGate p q {
    if p == true or q == true {
        write true
    } else {
        write false
    }
}

// description : calculates `x` AND `y`.
// writes 0    : `true` or `false` to the current position on the tape if
//               both `x` and `y` are true.
// arg p       : a 1-bit operand to the gate.
// arg q       : a 1-bit operand to the gate.
// complexity  : O(1)
func andGate p q {
    if p == true and q == true {
        write true
    } else {
        write false
    }
}

// description : calculates `x` XOR `y`.
// writes 0    : `true` or `false` to the current position on the tape if both
//               `x` and `y` are true.
// arg p       : a 1-bit operand to the gate.
// arg q       : a 1-bit operand to the gate.
// complexity  : O(1)
func xorGate p q {
    // Note that 'p XOR q' is equivalent to '(p OR q) AND (NOT(p AND q))'.
    // a = p OR q
    orGate p q
    let a = read

    // b = p AND q
    andGate p q
    let b = read

    // c = NOT b
    notGate b
    let c = read

    // d = a AND c
    andGate a c
}

// effect     : performs binary addition of two bits.
// writes 0   : the sum but of `p` add `q`.
// writes 1   : the carry bit of `p` add `q`
// arg a      : a 1-bit operand to the half adder.
// arg b      : a 1-bit operand to the half adder.
// complexity : O(1)
// ref        : http://www.electronics-tutorials.ws/combination/comb_7.html
func halfAdder a b {
    // The sum bit is computed as 'a XOR b'.
    xorGate a b

    // The carry bit is computed as 'a AND b'.
    right
    andGate a b
}

// effect     : performs binary addition of three binary bits.
// writes 0   : the sum bit of the addition.
// writes 1   : the carry-out bit of the addition.
// arg a      : a 1-bit operand to the half adder.
// arg b      : a 1-bit operand to the half adder.
// arg cIn    : a 1-bit carry-in from a previous addition.
// complexity : O(1)
// ref        : http://www.electronics-tutorials.ws/combination/comb_7.html
func fullAdder a b cIn {
    halfAdder a b
    // ..|sum0|*carry0|..
    let carry0 = read
    left
    let sum0 = read

    halfAdder sum0 cIn
    // ..|sum1|*carry1|..

    // The final carry is computed as the OR of the carry-out from both half
    // adders.
    orGate read carry0
}

// effect     : performs binary addition of two 4-bit integers.
// writes 0-3 : each bit of the result from from LSB to MSB.
// arg a0-a3  : each bit of the first operand from LSB to MSB.
// arg b0-b3  : each bit of the second operand from LSB to MSB.
// complexity : O(1)
// ref        : http://www.electronics-tutorials.ws/combination/comb_7.html
func rippleCarryAdder a3 a2 a1 a0 b3 b2 b1 b0 {
    fullAdder a3 b3 false
    // ..|sum3|*carry3|..

    fullAdder a2 b2 read
    // ..|sum3|sum2|*carry2|..

    fullAdder a1 b1 read
    // ..|sum3|sum2|sum1|*carry1|..

    fullAdder a0 b0 read
    // ..|sum3|sum2|sum1|sum0|*carry3|..
}

func main {
    // Get the operands a0-a3 from the tape.
    let a3 = read
    right
    let a2 = read
    right
    let a1 = read
    right
    let a0 = read
    right

    // Ignore the cell between the operands.
    right

    // Get the operands b0-b3 from the tape.
    let b3 = read
    right
    let b2 = read
    right
    let b1 = read
    right
    let b0 = read
    right

    // Write the result of the addition.
    write '='
    right
    rippleCarryAdder a3 a2 a1 a0 b3 b2 b1 b0
}

main
