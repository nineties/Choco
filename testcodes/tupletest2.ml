let make_tuple a b c = (a, b, c) in
let x = make_tuple 1 2 true in
let y = make_tuple 1.0 3 (1, 2) in
let (_, a, _) = x in
let (_, _, b) = y in
let (c, _) = b in
(print_char a; print_char c)
