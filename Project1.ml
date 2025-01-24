exception EmptySequence;;

type 'base sequence = 
  Empty | 
  Node of 'base * 'base sequence ;;

  (* let cons a b = 
    Node (a , b) ;;

    let rec append a b = 
      match a
      with Empty -> b |
      Node (x, y) -> cons x (append y b) ;; *) (* These Functions did not end up being necessary for the sort alogorithm I developed*)

  let rec split u = 
    match u
    with Empty -> (Empty, Empty) |
    Node (head1, tail1) -> 
      match tail1
      with Empty -> (Node (head1, Empty), Empty) |
      Node (head2, tail2) -> 
        let (left, right) = split tail2 in
        (Node (head1, left), Node(head2, right))

  let rec combine l r = 
    match l, r
    with Empty, Empty -> Empty |
    Empty, _ -> r |
    _, Empty -> l |
    Node (headl,taill), Node (headr ,tailr) -> 
    if (headl <= headr)
    then Node ((headl), combine taill r)
    else 
      Node (headr, combine l tailr);;

let rec sort u = 
  match u
  with Empty -> raise EmptySequence |
  Node (first, Empty) ->  u|
  Node (first, rest) -> 
    let (left, right) = split u  in
    combine (sort left) (sort right) ;;




let t = Node(Empty, Empty);;

 let s = Node(5, Node(17,Node(3,Node(2,Node(1,Node(289,Node(89,Empty)))))));;

let s = sort s;; 
 
let t = sort t;;


  


  


  