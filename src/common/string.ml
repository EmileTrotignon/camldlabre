module Set = struct
  include Set.Make (Stdlib.String)

  let unions sets = List.fold_left union empty sets
end

module Map = struct
  include Map.Make (Stdlib.String)

  let domain m = fold (fun key _data acc -> Set.add key acc) m Set.empty
end

include Stdlib.String