module MyList = struct
  type 'a t = 'a list = [] | (::) of 'a * 'a list
  
  (** [length]: [lst]の長さ
      ヘルパー関数で末尾再帰を実装してる
   *)
  let length lst = 
    let rec length_helper aux = function
      [] -> aux
    | _::l -> length_helper (aux + 1) l in
    length_helper 0 lst

  (** [compare_lengths]: [l1]と[l2]の長さを比べ同じなら[0],前者が長い場合[1],後者の場合[-1]を返す
   *)
  let rec compare_lengths l1 l2 = 
    match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | _ :: l1, _:: l2 -> compare_lengths l1 l2
    (*再帰的に同じ変数名を使うのなんか怖いけどわかりやすくもあるね*)
  
  (**[compare_lengths_with]: [lst]の長さと[len]を比べて[-1,0,1]を返す*)
  let rec compare_lengths_with lst len =
    match lst, len with
    | [], 0 -> 0
    | [], _ -> -1
    | _, 0 -> 1
    | _ :: lst , len -> compare_lengths_with lst (len - 1)
end
