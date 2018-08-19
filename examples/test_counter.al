import counter
import test

// Test a counter that has not reached zero.
proc test_counter_not_zero {
    let is_z = ""

    let c = Counter "123"
    ctr_start c
    ctr_dec c is_z
    ctr_dec c is_z

    assert_tape_eq is_z "0" "counter_not_zero"
}
test_counter_not_zero

// Test a counter that has reached zero.
proc test_counter_zero {
    let is_z = ""

    let c = Counter "123"
    ctr_start c
    ctr_dec c is_z
    ctr_dec c is_z
    ctr_dec c is_z

    assert_tape_eq is_z "1" "counter_zero_zero"
}
test_counter_zero

// Tests a counter that has reached zero can be reused.
proc test_counter_reuse {
    let is_z = ""

    let c = Counter "123"
    ctr_start c
    ctr_dec c is_z
    ctr_dec c is_z
    ctr_dec c is_z

    ctr_start c
    ctr_is_zero c is_z

    assert_tape_eq is_z "0" "counter_reuse"
}
test_counter_reuse
