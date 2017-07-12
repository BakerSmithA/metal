
import Marking

// summary: a collection of functions for movement over the tape.

// Aliases for the representation of left and right.
let leftDir  = 'l'
let rightDir = 'r'

// effect     : moves the read-write head left if `dir` is 'l', or right
//				otherwise.
// complexity : O(1)
func move dir {
	if dir == leftDir {
		left
	} else {
		right
	}
}

// effect     : moves the read-write head left if `dir` is 'l', or right if
//				otherwise, until `char` is read. The function terminates with
//				the read-write head on the cell containing `char`.
// warning    : causes an infinite loop if `char` is not encountered.
// complexity : O(1)
func moveUntil dir char {
	while read != char {
		move dir
	}
}

// effect     : moves the read-write head left until `char` is reached. The
//			    function terminates with the read-write head on the cell
//				containing `char`.
// warning    : causes an infinite loop if `char` is not encountered.
// complexity : O(n)
func leftUntil char {
	moveUntil leftDir char
}

// effect     : moves the read-write head right until `char` is reached. The
//			    function terminates with the read-write head on the cell
//				containing `char`.
// warning    : causes an infinite loop if `char` is not encountered.
// complexity : O(n)
func rightUntil char {
	moveUntil rightDir char
}

// effect     : moves the read-write head one cell to the left.
// note       : this is an alias for `left`.
// complexity : O(1)
func ignoreLeft {
	left
}

// effect     : moves the read-write head one cell to the right.
// note       : this is an alias for `right`.
// complexity : O(1)
func ignoreRight {
	right
}

// effect     : moves the read-write head to the start of the tape.
// warning    : employs marking.
// complexity : O(n)
func goToStart {
	// TODO: Use import statement to import tape.al
	let markSym = '#'
	let saved   = read

	write markSym
	left

	if read == markSym {
		// The head is zeroed.
		write saved

	} else {
		// The head is not zeroed, therefore put back the saved character.
		right
		write saved

		left
		goToStart
	}
}

// effect     : moves the read-wrute head to the end of input on the tape, i.e.
//			    until a space symbol is reached.
// complexity : O(n)
func goToEnd {
	rightUntil ' '
}
