type +'a struct'
type +'a ptr
type +'a const_ptr

type void = unit
type cstr = char const_ptr
type typeenc = cstr
type uint8_t = int
type ptrdiff_t = int

type objc_object
type objc_selector
type objc_ivar
type objc_class
type objc_category
type objc_method
type objc_property
type objc_property_attribute (* { const char *name; const char *value } *)
type objc_method_description (* { SEL name; char *types } *)

type id = objc_object struct' ptr
type sel = objc_selector struct' ptr
type ivar = objc_ivar struct' ptr
type class' = objc_class struct' ptr
type category = objc_category struct' ptr
type method' = objc_method struct' ptr
type imp = void ptr
type size_t = int
type objc_property_t = objc_property struct'
type objc_property_attribute_t = objc_property_attribute struct'
type alignment_t = int
type protocol = objc_object struct'

external malloc : size_t -> 'a ptr = "ObjC_malloc"
external free : 'a ptr -> unit = "ObjC_free"
external make_null_value : unit -> 'a ptr = "ObjC_make_null_value" 
let null = make_null_value ()

module Class =
  struct
    type t = class'
    type version = int32

    external get_name : t -> cstr =
      "Class_get_name"
    external is_meta_class : t -> bool =
      "Class_is_meta_class"
    external get_superclass : t -> t =
      "Class_get_superclass"
    external set_superclass : t -> t =
      "Class_set_superclass" (* DEPRECATED! *)

    external get_version : t -> version =
      "Class_get_version"
    external set_version : t -> version -> unit =
      "Class_set_version"

    external get_instance_size : t -> size_t =
      "Class_get_instance_size"

    external get_instance_variable : t -> cstr -> ivar =
      "Class_get_instance_variable"
    external get_class_variable : t -> cstr -> ivar =
      "Class_get_class_variable"
    external copy_ivar_list : t -> ivar array = 
      "Class_copy_ivar_list"

    external get_instance_method : t -> sel -> method' =
      "Class_get_instance_method"
    external get_class_method : t -> sel -> method' =
      "Class_get_class_method"
    external get_method_implementation : t -> sel -> imp =
      "Class_get_method_implementation"
    external get_method_implementation_stret : t -> sel -> imp =
      "Class_get_method_implementation_stret"
    external responds_to_selector : t -> sel -> bool =
      "Class_responds_to_selector"
    external copy_method_list : t -> int ref -> method' ptr =
      "Class_copy_method_list"

    external conforms_to_protocol : t -> protocol ptr -> bool =
      "Class_conforms_to_protocol"
    external copy_protocol_list : t -> protocol ptr array =
      "Class_copy_protocol_list"

    external get_property : t -> cstr -> objc_property_t =
      "Class_get_property"
    external copy_property_list : t -> int ref -> objc_property_t ptr =
      "Class_copy_property_list"

    external get_ivar_layout : t -> uint8_t const_ptr =
      "Class_get_ivar_layout"
    external get_weak_ivar_layout : t -> uint8_t const_ptr =
      "Class_get_weak_ivar_layout"

    external create_instance : t -> size_t -> id =
      "Class_create_instance"
    external construct_instance : t -> void ptr -> id =
      "Class_construct_instance"
    external destruct_instance : id -> void ptr =
      "Class_destruct_instance"
	
    external add_method : t -> sel -> imp -> cstr -> bool =
      "Class_add_method"
    external replace_method : t -> sel -> imp -> cstr -> imp =
      "Class_replace_method"
    external add_ivar : t -> cstr -> size_t -> alignment_t -> cstr -> bool =
      "Class_add_ivar"
    external add_protocol : t -> protocol ptr -> bool =
      "Class_add_protocol"
    external add_property : t -> cstr -> objc_property_attribute_t const_ptr -> int -> bool =
      "Class_add_property"
    external replace_property : t -> cstr -> objc_property_attribute_t const_ptr -> int -> void =
      "Class_replace_property"
    external set_ivar_layout : t -> uint8_t -> void =
      "Class_set_ivar_layout"
    external set_weak_ivar_layout : t -> uint8_t -> void =
      "Class_set_weak_ivar_layout"

    external get_image_name : t -> cstr =
      "Class_get_image_name"
  end

module TypeEnc =
  struct
    type t = typeenc
  end

module Method =
  struct
    type t = method'
    type description = objc_method_description struct'

    val get_name : t -> sel
    val get_implementation : t -> imp
    val get_type_encoding : t -> TypeEnc.t

    val get_number_of_arguments : t -> int
    val copy_return_type : t -> char ptr
    val copy_argument_type : t -> int -> char ptr
    (* val get_return_type : t -> char ptr -> size_t -> unit *)
    (* val get_argument_type : t -> int -> char ptr -> size_t -> unit *)
    val get_description : t -> description ptr

    val set_implementation : t -> imp -> imp
    val exchange_implementation : t -> t -> unit
  end

module Ivar =
  struct
    type t = ivar

    (* val get_name : t -> cstr *)
    (* val get_type_encoding : t -> TypeEnc.t *)
    (* val get_offset : t -> ptrdiff_t *)
  end

module Property =
  struct
    type t = objc_property_t
    type attribute = objc_property_attribute_t

    (* val get_name : t -> cstr *)
    (* val get_attributes : t -> cstr *)
    (* val copy_attribute_list : t -> int ref -> attribute ptr (\* 10.7 *\) *)
    (* val copy_attribute_value : t -> cstr -> char ptr (\* 10.7 *\) *)
  end

module Protocol =
  struct
    (* type t = protcol *)
    type p = protocol ptr

    (* val conforms_to_protocol : p -> p -> bool *)
    (* val is_equal : p -> p -> bool *)
    (* val get_name : p -> cstr *)
    (* val get_method_description : p -> sel -> req:bool -> inst:bool -> Method.description *)
    (* val copy_method_description_list : p -> req:bool -> inst:bool -> int ref -> Method.description ptr *)
    (* val get_property : p -> cstr -> req:bool -> inst:bool -> Property.t *)
    (* val copy_property_list : p -> int ref -> Property.t ptr *)
    (* val copy_protocol_list : p -> int ref -> p ptr *)

    (* val add_method_description : p -> sel -> TypeEnc.t -> req:bool -> inst:bool -> unit (\* 10.7 *\) *)
    (* val add_protocol : p -> p -> unit (\* 10.7 *\) *)
    (* val add_property : p -> cstr -> Property.attribute ptr -> int -> req:bool -> inst:bool -> void (\* 10.7 *\) *)
  end

module Sel =
  struct
    type t = sel

    (* val get_name : t -> cstr *)
    (* val get_uid : cstr -> sel *)
    (* val register_name : cstr -> sel *)
    (* val is_equal : sel -> sel -> bool *)
  end
    
module Object =
  struct
    type t = id

    (* val copy : t -> size_t -> t *)
    (* val dispose : t -> t *)

    (* val get_class : t -> Class.t -> Class.t *)
    (* val set_class : t -> Class.t -> Class.t *)

    (* val get_class_name : t -> cstr *)
    (* val get_indexed_ivars : t -> void ptr *)

    (* val get_ivar : t -> Ivar.t -> t *)
    (* val set_ivar : t -> Ivar.t -> t -> unit *)

    (* val set_instance_variable : t -> cstr -> void ptr -> Ivar.t *)
    (* val get_instance_variable : t -> cstr -> void ptr * Ivar.t *)
  end

(* val allocate_class_pair : Class.t -> cstr -> size_t -> Class.t *)
(* val register_class_pair : Class.t -> unit *)
(* val duplicate_class : Class.t -> cstr -> size_t -> Class.t *)
(* val dispose_class_pair : Class.t -> unit *)

(* val copy_image_names : unit -> int ref -> cstr ptr *)
(* val copy_clas_names_for_image : cstr -> int ref -> cstr ptr *)
    
(* val allocate_protocol : cstr -> Protocol.p (\* 10.7 *\) *)
(* val register_protocol : Protocol.p -> unit (\* 10.7 *\) *)

(* (\* val objc_enumeration_mutation : id -> unit *\) *)
(* (\* val objc_set_enumeration_mutation_handler : (id -> unit) -> unit *\) *)

(* (\* val objc_set_forward_handler : void ptr -> void ptr -> unit *\) *)

(* (\* val imp_implementation_with_block : void ptr -> imp *\) *)
(* (\* val imp_get_block : imp -> void ptr *\) *)
(* (\* val imp_remove_block : imp -> bool *\) *)

(* type association_policy = Assign | Retain_Nonatomic | Copy_Nonatomic | Retain | Copy *)

(* val set_associated_object : id -> cstr -> id -> association_policy -> void *)
(* val get_associated_object : id -> cstr -> id *)
(* val remove_associated_objects : id -> unit *)
    
(* val get_class : cstr -> id *)
(* val get_meta_class : cstr -> id *)
(* val lookup_class : cstr -> id *)
(* val get_required_class : cstr -> id *)
(* val get_future_class : cstr -> id *)
(* val set_future_class : Class.t -> cstr -> unit *)
(* val get_class_list : Class.t ptr -> int -> int *)
(* val get_protocol : cstr -> Class.t ptr *)
(* val copy_protocol_list : unit -> int ref -> Protocol.p ptr *)
