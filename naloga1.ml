(* a *)

let odstej_trojici (a1, a2, a3) (b1, b2, b3) = 
  (a1-b1, a2-b2, a3-b3)

(* b *)

let rec max_rezultat_do_n f n =
  (* vrnemo maximum od f(0), f(1), f(2), ..., f(n) *)
  if n = 0 then f 0 else max (f n) (max_rezultat_do_n f (n-1))


(* c *)

let pocisti_seznam opt_list =
  let rec pocisti acc = function
    | [] -> List.rev acc
    | None :: xs -> pocisti acc xs
    | Some x :: xs -> pocisti (x :: acc) xs
  in
  pocisti [] opt_list



(* d *)

let preveri_urejenost list =
  let rec je_urejen = function
    | [] -> true
    | x :: [] -> true
    | x1 :: x2 :: xs -> x1 <= x2 && je_urejen (x2 :: xs)
  in
  let lihi = List.filter (fun x -> x mod 2 = 1) list in
  let sodi = List.filter (fun x -> x mod 2 = 0) list in
  je_urejen sodi && je_urejen (List.rev lihi)

