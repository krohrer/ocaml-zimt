type id (* = objc_object struct ptr *)
type sel (* = objc_selector struct ptr *)
type ivar (* = objc_ivar ptr *)
type class' (* = objc_class ptr *)
type category (* = objc_category *)
type method' (* = objc_method ptr *)
type imp
type size_t = int
type objc_property_t
type objc_property_attribute_t (* = objc_property_attribute struct :: { const char *name; const char *value; } *)
type objc_method_description_t (* = objc_method_description struct :: { SEL name; char *types; } *)
type alignment_t = int
type protocol_t (* = protocol ptr *)

type 'a ptr
type 'a const_ptr

type void = unit
type cstr = char const_ptr
type typeenc = cstr
type uint8_t = int
type ptrdiff_t = int

val null : 'a ptr

module Class :
    sig
      type t = class'
      type version = int32

      val get_name : t -> cstr
      val is_meta_class : t -> bool
      val get_super_class : t -> t
      val set_super_class : t -> t (* DEPRECATED! *)

      val get_version : t -> version
      val set_version : t -> version -> unit

      val get_instance_size : t -> size_t

      val get_instance_variable : t -> cstr -> ivar
      val get_class_variable : t -> cstr -> ivar
      val copy_ivar_list : t -> int * ivar ptr

      val get_instance_method : t -> sel -> method'
      val get_class_method : t -> sel -> method'
      val get_method_implementation : t -> sel -> imp
      val get_method_implementation_stret : t -> sel -> imp
      val responds_to_selector : t -> sel -> bool
      val copy_method_list : t -> int * method' ptr

      val conforms_to_protocol : t -> protocol_t -> bool
      val copy_protocol_list : t -> int * protocol_t ptr

      val get_property : t -> cstr -> objc_property_t
      val copy_property_list : t -> int * objc_property_t ptr

      val get_ivar_layout : t -> uint8_t const_ptr
      val get_weak_ivar_layout : t -> uint8_t const_ptr

      val create_instance : t -> size_t -> id
      val construct_instance : t -> void ptr -> id
      val destruct_instance : id -> void ptr
	  
      val add_method : t -> sel -> imp -> cstr -> bool
      val replace_method : t -> sel -> imp -> cstr -> imp
      val add_ivar : t -> cstr -> size_t -> alignment_t -> cstr -> bool
      val add_protocol : t -> protocol_t -> bool
      val add_property : t -> cstr -> objc_property_attribute_t const_ptr -> int -> bool
      val replace_property : t -> cstr -> objc_property_attribute_t const_ptr -> int -> void
      val set_ivar_layout : t -> uint8_t -> void
      val set_weak_ivar_layout : t -> uint8_t -> void

      val get_image_name : t -> cstr
    end

val allocate_class_pair : class' -> cstr -> size_t -> class'
val register_class_pair : class' -> unit
val duplicate_class : class' -> cstr -> size_t -> class'
val dispose_class_pair : class' -> unit

val copy_image_names : unit -> int * cstr ptr
val copy_clas_names_for_image : cstr -> int * cstr ptr
    
module Method :
    sig
      type t = method'
      type description = objc_method_description_t
	    
      val get_name : t -> sel
      val get_implementation : t -> imp
      val get_type_encoding : t -> typeenc

      val get_number_of_arguments : t -> int
      val copy_return_type : t -> char ptr
      val copy_argument_type : t -> int -> char ptr 
      val get_return_type : t -> char ptr -> size_t -> unit
      val get_argument_type : t -> int -> char ptr -> size_t -> unit
      val get_description : t -> description ptr

      val set_implementation : t -> imp -> imp
      val exchange_implementation : t -> t -> unit
    end

module Ivar :
    sig
      type t = ivar

      val get_name : t -> cstr
      val get_type_encoding : t -> typeenc
      val get_offset : t -> ptrdiff_t
    end

module Property :
    sig
      type t = objc_property_t

      val get_name : t -> cstr
      val get_attributes : t -> cstr
      val copy_attribute_list : t -> int * objc_property_attribute_t ptr (* 10.7 *)
      val copy_attribute_value : t -> cstr -> char ptr (* 10.7 *)
    end

module Protocol :
    sig
      type t = protocol_t

      val conforms_to_protocol : t -> t -> bool
      val is_equal : t -> t -> bool
      val get_name : t -> cstr
      val get_method_description : t -> sel -> req:bool -> inst:bool -> objc_method_description_t
      val copy_method_description_list : protocol_t -> req:bool -> inst:bool -> int * objc_method_description_t ptr
      val get_property : t -> cstr -> req:bool -> inst:bool -> objc_property_t
      val copy_property_list : t -> int * objc_property_t ptr
      val copy_protocol_list : t -> int * protocol_t ptr

      val add_method_description : t -> sel -> typeenc -> req:bool -> inst:bool -> unit (* 10.7 *)
      val add_protocol : t -> t -> unit (* 10.7 *)
      val add_property : t -> cstr -> objc_property_attribute_t ptr -> int -> req:bool -> inst:bool -> void (* 10.7 *)
    end

val allocate_protocol : cstr -> protocol_t (* 10.7 *)
val register_protocol : protocol_t -> unit (* 10.7 *)

module Sel : 
    sig
      type t = sel

      val get_name : t -> cstr
      val get_uid : cstr -> sel
      val register_name : cstr -> sel
      val is_equal : sel -> sel -> bool
    end

(* val objc_enumeration_mutation : id -> unit *)
(* val objc_set_enumeration_mutation_handler : (id -> unit) -> unit *)

(* val objc_set_forward_handler : void ptr -> void ptr -> unit *)

(* val imp_implementation_with_block : void ptr -> imp *)
(* val imp_get_block : imp -> void ptr *)
(* val imp_remove_block : imp -> bool *)

type objc_association_policy = Assign | Retain_Nonatomic | Copy_Nonatomic | Retain | Copy

val set_associated_object : id -> cstr -> id -> objc_association_policy -> void
val get_associated_object : id -> cstr -> id
val remove_associated_objects : id -> unit
    
module Object :
    sig
      type t = id
      val copy : t -> size_t -> t
      val dispose : t -> t

      val get_class : t -> class' -> class'
      val set_class : t -> class' -> class'

      val get_class_name : t -> cstr
      val get_indexed_ivars : t -> void ptr

      val get_ivar : t -> ivar -> t
      val set_ivar : t -> ivar -> t -> unit

      val set_instance_variable : t -> cstr -> void ptr -> ivar
      val get_instance_variable : t -> cstr -> void ptr * ivar
    end

val get_class : cstr -> id
val get_meta_class : cstr -> id
val lookup_class : cstr -> id
val get_required_class : cstr -> id
val get_future_class : cstr -> id
val set_future_class : class' -> cstr -> unit
val get_class_list : class' ptr -> int -> int
val get_protocol : cstr -> class' ptr
val copy_protocol_list : unit -> int * protocol_t ptr

