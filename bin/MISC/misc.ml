module MyTree = struct
  type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a

end

module MyStack = struct
  (** 再帰的なデータ型には名前をつけるっぽい？*)
  type 'a t = { mutable c: 'a list; mutable len: int}

  exception Empty
  
  (**[create: unit -> 'a t]*)
  let create () = { c = []; len = 0}
  (**[clear: 'a t -> 'a t]*)
  let clear s = s.c <- []; s.len <- 0
  
  let copy s = { c = s.c; len = s.len}

  let push x s = s.c <- x::s.c; s.len <- s.len + 1
  let pop_opt s = 
    match s.c with
    | hd::tl -> s.c<-tl;s.len <- s.len -1; Some hd
    | [] -> None


end

module Queue = struct
  (**補助的なデータ型?*)
  type 'a cell =
    | Nil
    | Cons of { content: 'a; mutable next :'a cell}

  type 'a t = {
    mutable length: int;
    mutable first: 'a cell;
    mutable last: 'a cell
    }
  
  let add x q =
    let cell = Cons {
      content = x;
      next = Nil
    } in
    match q.last with
    | Nil ->
        q.length <- 1;
        q.first <- cell;
        q.last <- cell
    | Cons last ->
        q.length <- q.length + 1;
        last.next <- cell;
        q.last <- cell
end
