module X :
  sig
    type 'a t
    module type T = sig type w end
    module Make (T : T) : T with type w = T.w

    val make : 'a t -> (module T with type w = 'a)
  end
  =
  struct
    type 'a t = 'a
    module type T = sig type w end
    module Make (T : T) =
      struct
	type w = T.w
      end

    let make (x : 'a t) = (module Make(struct type w = 'a end) : T)
  end
