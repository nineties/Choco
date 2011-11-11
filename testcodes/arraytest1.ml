let a = create_array 10 0 

let rec fill a n = 
    if n >= 10
        then ()
        else (a.(n) <- 2 * n; fill a (n + 1))

let _ = (fill a 0; print_char a.(9))
