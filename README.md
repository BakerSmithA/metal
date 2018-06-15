# Metal 

Metal is a programming language used to describe the function of a Turing machine. This means you're not given a lot of help to do what would normally be essential parts of a language, such as maths. However, since the language is Turing complete any possible computation can still be performed, even with a limited toolset.

# Turing Machine

The machine operates on a tape which extends infinitely to the right. A read-write head can move left and right along the tape, reading and writing symbols to it. They can also 'accept' or 'reject' depending on the input.

In Metal, any ASCII symbol can be stored on the tape. The code is used to describe how the head should move across the tape, and when to read or write, and when to accept or reject.

# Hello World
Our first program will be the classic "Hello World" message. Here's the full source code.

```c 
// hello_world.al
import io

printAll "Hello World!"
```

To run the program, put the code in a `hello_world.al` file and run using `metal  hello_world`. This runs an interpreter which executes the code.

```sh
$ metal hello_world
Hello World!
Accepted
```

# Reading
To read from a tape, the `read` function is used. This takes as an argument the tape to read from. 

```c
// read.al
print (read main)
```

The program above reads the symbol underneath the head of the `main` tape. The `main` tape is used to give data into the program, and its contents comes from the second command line argument. Initially, the position of the head is in the leftmost position.

```sh
$ metal read "abc"
a
Accepted
```

# Writing
To write to the tape, the `write` function is used. This takes the tape to write to as the first argument, and the symbol to write as the second.

```c
// write.al
write main 'X'
print (read main)
```

Running the program, we can see that even though the first letter of input was an `a`, the output is an `X` since the first symbol on the tape was overwritten.

```sh
$ metal write "abc"
X
Accepted
```

# Moving the Read-Write Head
To move the head, the builtin `left` and `right` functions are used. These take one argument which is the head of which tape to move. 

```c
// move.al
right main
right main
left main
print (read main)
```

The output of the program will be the second symbol on the tape.

```sh
$ metal move "abc"
b
Accepted
```

The head can be moved infinitely to the right, however, it cannot be moved left further than the start position. This is shown in `left.al` below. However many times `left` is called, the beginning symbol will always be printed.

```c
// left.al
left main 
left main
print (read main)
```

```sh
$ metal left "abc"
a
Accepted
```

It is possible to move the tape past the input. Reading from this area results in a space, i.e. the `' '` character.

```c
// space.al
right main
right main
print (read main)
```

```sh
$ metal space "ab"

Accepted
```

# Accepting and Rejecting

Accepting and rejecting are done using the `accept` and `reject` keywords respectively. They immediately halt the TM and cause the program to exit. If neither if encountered before the end of the program the TM accepts, and hence `Accepted` is given as output to the shell.

# Variables

## Tape Symbols
It is common to want to read a symbol from the tape, overwrite it, move around to perform some computation, and then finally write back the same symbol. 

One such example is moving to the start of a tape. To do this a marking symbol (e.g. `'#'`) is written to the tape. The TM then tries to move left towards the start. A read is then performed to check whether the move was successful. If the `'#'` is read then we know we are at the start of the tape. Otherwise, we are not at the start and so the `'#'` must be replaced with the original symbol. We can then try moving left again.

In TM state transition diagrams, the original character is saved as part of the state the TM is in. Translating this directly we would end up with:

```c
if (read main) == 'a' {
	write main '#'
	// Perform computation
	write main 'a'
}
else if (read main) == 'b' {
	write main '#'
	// Perform same computation
	write main 'b'
}
// ...
```

Here, the character that was overwritten is saved in the branch (i.e. state) that was taken. This is not nice as a branch is required for every single character we could want to save. Therefore, variables are used to save the state and simplify the program. Translating the snippet above we get:

```c
let saved = read main
// Perform computation
write main saved
```

### Literals

Symbol literals, or other symbol variables, can also be stored inside variables. Such as shown below.

```c
// sym_lit.al 
let sym_lit = 'a'
let sym_var = sym_lit

print sym_lit
print sym_var
```

```sh
$ metal sym_lit
aa
```

### Immutability

Since variables containing symbols are used to store state, they are immutable and cannot be changed after initialisation. 

```c
let x = 'a'

x = 'b' 
// Fails to compile.

let x = 'c' 
// Fails to compile as redefinition of x.
```

## Tapes
Tapes can also be stored in variables. This allows for the easy construction of Multitape Turing machines. The program below shows how to copy the first symbol from the main tape onto another tape we defined.

```c
// multi.al
import io

let tape = "xyz"
write tape (read main) // Copy
printAll tape
```

```sh
$ metal multi "abc"
ayz
```

### References

Tapes have reference semantics, and so if a variable assigned to an exisiting tape, and then that tape is modified, the original tape will also be modified. This behaviour is what allow functions such as `left`, `right`, and `write` to work.

```c
// ref_tape.al
let tape1 = "abc"
let tape2 = tape1

write tape2 'X'
printAll tape1 // Read from a different tape.
```

```sh
$ metal ref_tape
Xbc
```

# Booleans

- Can be used in if- and while-statements.
- Cannot be stored in variables, as are not part of the tape symbols.

# If statements

- Can have a single if, if-else, and if-elseif-else

# While 

# Functions

# Structs
