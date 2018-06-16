# Metal 

[![CircleCI](https://circleci.com/gh/BakerSmithA/metal.svg?style=svg)](https://circleci.com/gh/BakerSmithA/metal)

Metal is a programming language used to describe the function of a Turing machine. This means you're not given a lot of help to do what would normally be essential parts of a language, such as maths. However, since the language is Turing complete any possible computation can still be performed, even with a limited toolset.

# Turing Machines

The machine operates on a tape which extends infinitely to the right. A read-write head can move left and right along the tape, reading and writing symbols to it. They can also 'accept' or 'reject' depending on the input.

In Metal, any ASCII symbol can be stored on the tape. The code is used to describe how the head should move across the tape, and when to read or write, and when to accept or reject.

# Hello World
Our first program will be the classic "Hello World" message. Here's the full source code.

```c 
// hello_world.al
import io

print_all "Hello World!"
```

To run the program, put the code in a `hello_world.al` file and run using `metal  hello_world`. This runs an interpreter which executes the code.

```sh
$ metal hello_world.al
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
$ metal read.al "abc"
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
$ metal write.al "abc"
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
$ metal move.al "abc"
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
$ metal left.al "abc"
a
Accepted
```

It is possible to move the tape past the input. Reading from this area results in a space, i.e. the `' '` character.

```c
// space.al
right main
print (read main)
```

```sh
$ metal space.al "a"

Accepted
```

# Accepting and Rejecting

Accepting and rejecting are done using the `accept` and `reject` keywords respectively. They immediately halt the TM and cause the program to exit. If neither if encountered before the end of the program the TM accepts, and hence `Accepted` is given as output to the shell.

```c
// halt.al
print 'a'
reject
print 'b'
```

```sh
$ metal halt.al 
a
Rejected
```

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
$ metal sym_lit.al
aa
```

### Immutability

Since variables containing symbols are used to store state, they are immutable and cannot be changed after initialisation. Immutable variables also disallow anything other than tapes to be used to perform computations.

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
print_all tape
```

```sh
$ metal multi.al "abc"
ayz
```

### References

Tapes have reference semantics, and so if a variable assigned to an exisiting tape, and then that tape is modified, the original tape will also be modified. This behaviour is what allow functions such as `left`, `right`, and `write` to work.

```c
// ref_tape.al
import io

let tape1 = "abc"
let tape2 = tape1

write tape2 'X'
print_all tape1 // Read from a different tape.
```

```sh
$ metal ref_tape.al
Xbc
```

# If statements
Branching in Metal is performed using if-else statements. Note that you do not need parenthesis around the conditional. However, braces are required, even if the branch only contains one statement.

```c
if read main == 'a' {
	print '2'
} 
else if read main == 'a' && read tape != 'b' {
	print '2'
}
else {
	print '3'
}
```

## Scope
Inside the braces of one of the conditionals variables, functions, and struct definitions can be overwritten. When running the program, the innermost definition will be used.

```c
// scope.al
let x = 'a'
if True {
	let x = 'b'
	print x
}
```

```sh
$ metal scope.al 
b
```

# While 
While loops are Metal's only looping structure. It is executed until the loop condition becomes false. The same scoping rules that apply to if-statements also apply to while-statements. 

The program below shows how to print the character before the first `'a'`.

```c
// find.al

// Find the first 'a'.
while read main != 'a' {
	right main
}
// Move to the character before.
left main 
print (read main)
```

```sh
$ metal find.al "xyzabc"
z
```

# Functions
Functions in Metal have no return, however they can modify tapes and objects due to their reference semantics. Functions also require the type of the arguments to be explicitly stated. The same scope rules that apply to if-statements and while-statements also apply to functions.

Below is a function which takes a tape and a symbol as input. It then writes this symbol to the current and next positions on the tape. The function is then run by giving it's name followed by the required arguments.

```c
// write2.al
import io

func write2 t:Tape s:Sym {
	write t s 
	right t
	write t s
}

write_right main 'c'

print_all main
```

```sh
$ metal write2.al xyz
ccz
```

## Recursion
Functions may also be recursive. For example, the function below moves the read-write head to the start of the tape. Note that the symbol `'#'` is used to mark the position of the head. It is assumed that `'#'` is unique and therefore not used anywhere else.

```c
// zero.al

func zero t:Tape {
    let saved = read t
    // Mark the current position of the head.
    write t '#'

    // Try to move left. If we are at the start then this will have no effect.
    left t

    if read t == '#' {
        // The head did not move, therefore we're at the start.
        // This is the base case of the function.
        write t saved
    } 
    else {
        // The head did move, therefore we are not at the start.
        // We need to replace the overwritten symbol with the original and
        // then continue searching.
        right t
        write t saved
        left t
        left t

        // Recursive call.
        zero t
    }
}
```

To test out the function, we'll first move the read-write head to the right. Then we can check the head is moved back to the start.

```c
// zero.al (continued)
right main 
right main 
print (read main)

zero main
print (read main)
```

```sh
$ metal zero.al "abc"
ca
```

## Nested Functions
Functions need not only exist at the topmost level; they can be nested within other functions. This can be useful for hiding functions that don't need to be exposed to other users. The following shows the slightly contrived example of copying the contents of one tape to anothe tape.

```c
// nested_funcs.al
import io

func copy_all src:Tape dest:Tape {
	// Copies a single symbol from one tape to the other.
	func copy_move src:Tape dest:Tape {
		write dest (read src) 
		right src
		right dest
	}
	
	while read src != ' ' {
		copy_move src dest
	}
}

let t = ""
copy_all main t
print_all t
```

```sh
$ metal nested_funcs.al "xyz"
xyz
```

# Structs
Metal's structs are a typed collection of fields. They're useful for grouping data together. They are mutable, however, they do not contain functions but are instead acted upon by outside functions.

To motivate a possible usage, one problem that arises is ensuring that symbols used to mark a tape are unique (an example of marking the tape is given in *Functions: Recursion*). The marking symbols need to be unique to ensure the computation is performed correctly. 

To keep track of used marking symbols we will use a struct. The struct also contains the saved characters which have been overwritten with a mark.

```c
// struct.al
struct Marks {
	// Keeps track of symbols that have not yet been used to mark a tape.
	free:Tape
	// Keeps track of symbols that have been overwritten with marks.
	saved:Tape
}
```

To create an instance of the struct (an object) we use the constructor provided by the struct. This is a function which takes, as arguments, values for each member variable in the order they are defined in the struct.

Below, an object is created containing `free` initialised to the tape containing `#%$`. These are the symbols that can be used to mark a tape. `saved` has also been initialised with an empty tape.

```c
// struct.al (continued)
let marks = Marks "#%$" ""
```

Next, we'll define a function which is called when a user of the struct wants to mark a tape `t` with a marking symbol. The dot operator `.` is used to access members of an object. This can also be chained to access objects within other objects.

```c
// struct.al (continued)
func mark t:Tape marks:Marks {
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
```

We need another matching function which is used to put back used marks and overwritten symbols. 

```c
// struct.al (continued)
func unmark t:Tape marks:Marks {
	// Get the saved symbol and put it back in the tape.
	left marks.saved
	write t (read marks.saved)
	left marks.saved
	
	// Free the used mark.
	left marks.free
}
```

These two functions must be called in pairs to ensure the state is correctly maintained. Below shows some example usage.

```c
// struct.al (continued)
mark main marks
// Perform computation...
mark main marks 
// Perform another, nested computation...
// Then put back the used symbol and replaced symbol on the main tape.
unmark main marks
unmark main marks
```

# Imports

Importing other files is done using the `import` keyword followed by the path from the current file to the imported file. This allows variables, functions, and structs to be shared among files. Clashes or redefinitions, however, will cause compilation errors. Given the file structure:

```
.
├── dir
│   └── file3.al
├── file1.al
└── file2.al
```

File 1 can import files 2 and 3 in the following way.

```c
// file1.al
import file1
import dir/file2

print x
print y
```

```c
// file2.al
let x = 'a'
```

```c
// file3.al
let y = 'b'
```

Running file 1 we get the following output.

```sh
$ metal file1.al 
ab
```