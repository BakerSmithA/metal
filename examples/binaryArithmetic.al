
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

// effect     : moves the head to the next digit to check in the first operand.
// complexity : O(n)
func moveToFirstOp {
    // Move to the end of first operand.
    while read != '+' {
        left
    }
    left
    // '#' indicates the digits that have been checked.
    while read != '#' {
        left
    }
    right
}

// effect     : moves the head to the next digit to check in the second operand.
// complexity : O(n)
func moveToSecondOp {
    // Move to the start of the second operand.
    while read != '+' {
        right
    }
    right
    // '#' indicates the digits that have been checked.
    while read == '#' {
        right
    }
}

// effect     : move to the very right of the tape, except one. This is the
//              position of the last written carry bit.
// complexity : O(n)
func moveToLastCarry {
    // Move to the end of the tape.
    while read != ' ' {
        right
    }
    left
}

// effect     : calculates the sum of the bits on the tape.
// complexity : O(n)
func add cIn {
    // Stop if the end of the first operand has been reached.
    if read == '+' {
        accept
    }

    // Read the next bit of the first operand.
    let first = read
    write '#'

    // Read the next bit of the second operand.
    moveToSecondOp
    let second = read
    write '#'

    // Write the answer
    moveToLastCarry
    fullAdder first second cIn

    // Read the carry bit from the tape to be used the next time add is called.
    let newCIn = read

    // Move to the unprocessed bits of the first operand and repeat.
    moveToFirstOp

    add newCIn
}

// effect     : moves the read-write head to the start (zeroed) position.
// complexity : O(n)
func zero {
	let mark  = '#'
	let saved = read

	write mark
	left

	if read == mark {
		// The head is zeroed.
		write saved

	} else {
		// The head is not zeroed, therefore go back
		// and put the saved character back.
		right
		write saved

		// Now continue searching for the start by moving to
		// the left and performing a recursive call.
		left
		zero
	}
}

// effect     : writes '=0' to the end of the tape.
// complexity : O(n)
func initAnswer {
    while read != ' ' {
        right
    }
    write '='
    right
    write false
}

initAnswer
zero
add false
