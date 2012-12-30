module type DEP =
  sig
    module type T = sig
      type t
      val dependencies : t list Lazy.t
    end
    module type A = sig
      module type T
    end
      
    module Make (A:A) : T with type t = (module A)
  end
