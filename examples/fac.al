import int

proc fac n:Int r:Int {
    set1_same_len r r

    let is_z = ""
    is_zero n is_z

    while read is_z == '0' {
        // Copy into x so n has the same number of bits as r.
        let x = Int ""
        set0_same_len n x
        copy_int n x

        check_dec_by_1 n is_z
        mult r x r
    }
}

let n = Int "101"
let r = Int "0000"

fac n r
int_print r
