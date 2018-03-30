# Metal

[![CircleCI](https://circleci.com/gh/BakerSmithA/metal.svg?style=svg)](https://circleci.com/gh/BakerSmithA/metal)

Metal is programming language used to describe the function of a Turing machine. Since the language is Turing complete, any possible computable problem can be implemented using basic tape operations, such as left, right, read, and write, and a handful of control structures.  

# Examples

Not to break tradition, a hello-world program is as simple as:

```c
print "Hello World"
```
Movement of the read-write head is done using the `left` and `right` commands. For example, after executing the program below, the read-write head is at position one (as indexing is done from zero).

```c
right
right
left
```

Reading from the tape is done using `read`, or is implicit in the `print` command when no arguments are given. A program to print each cell until a space character is encountered could be:

```c
while read != ' ' {
	print
	right
}
```

Modification of the tape is done using `write`. For example, to replace 'a' characters input on the tape with '#', you could do:

```c
while read != ' ' {
	if read == 'a' {
		write '#'
	}
	right
}
```

Functions are useful to execute the same code multiple times. For example, a function that moves the read-write head back to the start of the tape could be written as:

```c
func goToStart {
	// The symbol used to mark the position of the read-write head.
	let mark = '#'
	// We'll be overwriting the current cell, so we need to save it.
	let saved = read

	write mark
	left

	if read == mark {
		// The read-write head cannot move past the zero position, therefore if 
		// the cell is marked with a '#' we know the read-write head did not move.
		// Therefore we must be at the start.
		write saved

	} else {
		// Otherwise, we're not at the start so we need to put back the saved character.
		right
		write saved

		// Now continue searching for the start by moving to the left and
		// performing a recursive call to `goToStart`.
		left
		goToStart
	}
}
```
