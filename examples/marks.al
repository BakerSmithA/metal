import tape

// Stores remaining marks, and any symbols that were replaced.
// To get the next tape symbol use:
// > let next = read (marks.free)
struct Marks {
	// Keeps track of symbols that have not yet been used to mark a tape.
	free:Tape
	// Keeps track of symbols that have been overwritten with marks.
	saved:Tape
}

// effect     : initialises m to contain the given free symbols. m is given no
//              saved symbols.
// complexity : O(1)
proc init_marks ms:Marks free_syms:Tape {
    copy_until free_syms ' ' ms.free
    to_start ms.free
}

// effect     : marks the tape with the next available symbol.
// complexity : O(1)
proc mark t:Tape marks:Marks {
	// Save the symbol on the tape.
	write marks.saved (read t)
	// Move to the next position to avoid overwriting the saved symbol.
	right marks.saved

	// Overwrite the symbol on the tape.
	write t (read marks.free)
	// Move to the next available symbol. Therefore a different symbol
	// will be used when `mark` is called again.
	right marks.free
}

// effect     : unmarks the tape, replacing it with its saved symbol.
// complexity : O(1)
proc unmark t:Tape marks:Marks {
	// Get the saved symbol and put it back in the tape.
	left marks.saved
	write t (read marks.saved)
	left marks.saved

	// Free the used mark.
	left marks.free
}
