
// effect: moves the head to the next digit to check in the first operand.
func moveToFirstOp {
    // Move to the first operand.
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

// effect: moves the head to the next digit to check in the second operand.
func moveToSecondOp {
    // Move to the second operand.
    while read != '+' {
        right
    }
    right
    // '#' indicates the digits that have been checked.
    while read == '#' {
        right
    }
}

// effect: accepts if the tape contains a string in the form '{0|1}+{0|1}'.
func same {
    while read != '+' {
        let first = read
        write '#'

        moveToSecondOp

        if read != first {
            reject
        }

        write '#'
        moveToFirstOp
    }
}

same
