
open Code

(****)

let get_closures (_, blocks, _) =
  AddrMap.fold
    (fun _ block closures ->
       List.fold_left
         (fun closures i ->
            match i with
              Let (x, Closure (l, cont)) ->
                VarMap.add x (l, cont) closures
            | _ ->
                closures)
         closures block.body)
    blocks VarMap.empty

(****)

let rewrite_block (pc', handler) pc blocks =
  let block = AddrMap.find pc blocks in
  assert (block.handler = None);
  let block = { block with handler = handler } in
  let block =
    match block.branch with
      Return y -> { block with branch = Branch (pc', [y]) }
    | _        -> block
  in
  AddrMap.add pc block blocks

let (>>) x f = f x

(* Skip try body *)
let fold_children blocks pc f accu =
  let block = AddrMap.find pc blocks in
  match block.branch with
    Return _ | Raise _ | Stop ->
      accu
  | Branch (pc', _) | Poptrap (pc', _) ->
      f pc' accu
  | Pushtrap (_, _, (pc1, _), pc2) ->
      f pc1 (if pc2 >= 0 then f pc2 accu else accu)
  | Cond (_, _, (pc1, _), (pc2, _)) ->
      accu >> f pc1 >> f pc2
  | Switch (_, a1, a2) ->
      accu >> Array.fold_right (fun (pc, _) accu -> f pc accu) a1
           >> Array.fold_right (fun (pc, _) accu -> f pc accu) a2

let rec traverse f pc visited blocks =
  if not (AddrSet.mem pc visited) then begin
    let visited = AddrSet.add pc visited in
    let (visited, blocks) =
      fold_children blocks pc
        (fun pc (visited, blocks) ->
           let (visited, blocks) =
             traverse f pc visited blocks in
           (visited, blocks))
        (visited, blocks)
    in
    let blocks = rewrite_block f pc blocks in
    (visited, blocks)
  end else
    (visited, blocks)

let rewrite_closure blocks cont_pc clos_pc handler =
  snd (traverse (cont_pc, handler) clos_pc AddrSet.empty blocks)

(****)

(*
get new location
put continuation at new location
update closure body to return to this location
make current block continuation jump to closure body
*)

let inline closures live_vars blocks free_pc pc =
  let block = AddrMap.find pc blocks in
  let (body, (branch, blocks, free_pc)) =
    List.fold_right
      (fun i (rem, state) ->
         match i with
           Let (x, Apply (f, args, Some n))
               when n = List.length args
                 && live_vars.(Var.idx f) = 1
                 && VarMap.mem f closures ->
             let (params, (clos_pc, clos_args)) = VarMap.find f closures in
             let (branch, blocks, free_pc) = state in
             let blocks =
               AddrMap.add free_pc
                 { params = [x]; handler = block.handler;
                   body = rem; branch = branch } blocks
             in
             let blocks =
               rewrite_closure blocks free_pc clos_pc block.handler in
             let blocks =
               AddrMap.add (free_pc + 1)
                 { params = params; handler = block.handler;
                   body = []; branch = Branch (clos_pc, clos_args) } blocks
             in
             ([], (Branch (free_pc + 1, args), blocks, free_pc + 2))
         | _ ->
             (i :: rem, state))
      block.body ([], (block.branch, blocks, free_pc))
  in
  (AddrMap.add pc {block with body = body; branch = branch} blocks, free_pc)

(****)

let do_inline = ref true

let f ((pc, blocks, free_pc) as p) live_vars =
  if !do_inline then begin
    let closures = get_closures p in
    let (blocks, free_pc) =
      AddrMap.fold
        (fun pc _ (blocks, free_pc) ->
           inline closures live_vars blocks free_pc pc)
        blocks (blocks, free_pc)
    in
    (pc, blocks, free_pc)
  end else
    p
