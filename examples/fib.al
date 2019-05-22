import int

proc fib n:Int r:Int {
    let num1 = Int ""
    set0_same_len r num1
    inc_by_1 num1

    let num2 = Int ""
    set0_same_len r num2

    let is_z = ""
    is_zero n is_z

    while read is_z == '0' {
        _print r.bin
        check_dec_by_1 n is_z

        add num1 num2 r
        copy_int num2 num1
        copy_int r num2
    }
}

// Compute fib(70)
let n = Int "000110001"
//"00101"
let r = Int "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

fib n r

// Prints 111100010000101101100011001011001001010010110101000000000000
// Representing 190392490709135 in decimal
int_print r
