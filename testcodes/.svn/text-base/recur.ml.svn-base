(*
let rec fact x =
    let rec facti i c =
        if i > x then c else facti (i + 1) (c * i) in
    facti 2 1 in
print_int (fact 5)
*)

let rec fact x c i =
    if i > x then c else fact x (c * i) (i + 1) in
let rec fact x i c =
    if i > x then c else fact x (i + 1) (c * i) in
print_int (fact 5 2 1)
