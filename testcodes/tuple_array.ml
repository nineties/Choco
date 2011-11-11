(* これは3*10ワードの静的配列になるはず *)
let x = create_array 10 (1, 0.0, true)

let _ = (
    x.(0) <- (2, 1.0, false);
    let (a, _, _) = x.(0) in
    print_char a
    )
