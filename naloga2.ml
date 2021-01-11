type 'a gnezdenje =
  | Element of 'a
  | Podseznam of 'a gnezdenje list

(*  a  *)

let gnezdenje_primer =
  [
    Element 1; 
    Element 2; 
    Podseznam [
      Element 3; Podseznam [Element 4]; Podseznam []
      ];
    Podseznam [Element 5]
  ]

(*  b  *)

let rec najvecja_globina = function
  | [] -> 0
  | Element x :: gnezdenja -> 
      max 1 (najvecja_globina gnezdenja)
  | Podseznam sez_gnezdenj :: gnezdenja ->
      max (1 + najvecja_globina sez_gnezdenj) (najvecja_globina gnezdenja)


(*  c  *)

let rec preslikaj f = function
  | [] -> []
  | Element x :: gnezdenje -> 
      Element (f x) :: preslikaj f gnezdenje
  | Podseznam sez :: gnezdenje ->
      Podseznam (preslikaj f sez) :: preslikaj f gnezdenje

(*  d  *)

let rec splosci = function
  | [] -> []
  | Element x :: gnezdenje -> x :: splosci gnezdenje
  | Podseznam sez :: gnezdenje -> 
      (splosci sez) @ splosci gnezdenje


(*  e  *)

let rec alternirajoci_konstruktorji = function
  | [] | _ :: [] -> true
  | Element _ :: (Podseznam _ :: _ as rest) ->
      alternirajoci_konstruktorji rest
  | Podseznam _ :: (Element _ :: _ as rest) ->
      alternirajoci_konstruktorji rest
  | Element _ :: Element _ :: _ 
  | Podseznam _ :: Podseznam _ :: _ -> false

(*  f  *)

let rec zlozi_preko_gnezdenja f acc sez = 
  sez |> splosci |> List.fold_left f acc

let rec zlozi_preko_gnezdenja_tl_rec f acc sez =
  let rec zlozi acc todo_list = function
    | [] ->
        begin match todo_list with
        | [] -> acc
        | task :: tasks ->
            zlozi acc (tasks) task
        end
    | Element x :: gnezdenje ->
        zlozi (f acc x) todo_list gnezdenje
    | Podseznam sez :: gnezdenje ->
        zlozi acc (gnezdenje :: todo_list) sez
  in
  zlozi acc [] sez


(*

      1
     /  \
    2    3
  /     / \
 4     5   6

*)



