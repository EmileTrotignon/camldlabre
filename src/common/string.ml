module Set = struct
  include Set.Make (Stdlib.String)

  let unions sets = List.fold_left union empty sets
end

module Map = struct
  include Map.Make (Stdlib.String)

  let domain m = fold (fun key _data acc -> Set.add key acc) m Set.empty

  let size m = fold (fun _ _ -> ( + ) 1) m 0

  let of_bindings bds =
    List.fold_left (fun m (key, data) -> add key data m) empty bds
end

module Graph = struct
  module Self = Graph.Persistent.Digraph.Concrete (struct
    include Stdlib.String

    let hash = Hashtbl.hash
  end)

  module Dfs = Graph.Traverse.Dfs (Self)
  module Topological = Graph.Topological.Make (Self)
  include Self

  let add_edges v edges g = Set.fold (fun e g -> add_edge g v e) edges g

  let of_map m =
    Map.fold (fun v edges g -> add_edges v edges (add_vertex g v)) m empty
end

include Stdlib.String

let hash = Hashtbl.hash
