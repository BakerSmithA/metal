
// For testing, move the head to the right a bit.
right
right
right
right

// Moves the read-write head to the start (zeroed) position.
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

zero
