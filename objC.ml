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
type objc_property_t = objc_property struct' ptr
type objc_property_attribute_t = objc_property_attribute struct'
type alignment_t = int
type protocol = objc_object struct'


module C =
    struct
      external malloc : size_t -> 'a ptr = "C_malloc"
      external free : 'a ptr -> unit = "C_free"
      external make_null : unit -> 'a ptr = "C_make_null" 
      let null = make_null ()

      external copy_cstr : cstr -> string = "C_copy_cstr"
      external ptr_array_ith : 'a ptr ptr -> int -> 'a ptr = "C_ptr_array_ith"
      let ptr_array_iter f n parr =
	for i = 0 to n - 1 do
	  let p = ptr_array_ith parr i in
	  f p
	done
    end

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
    (* external set_superclass : t -> t = *)
    (*   "Class_set_superclass" (\* DEPRECATED! *\) *)

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
    external copy_ivar_list : t -> int ref -> ivar ptr = 
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
    external copy_protocol_list : t -> int ref -> protocol ptr ptr =
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
    external set_ivar_layout : t -> uint8_t const_ptr -> void =
      "Class_set_ivar_layout"
    external set_weak_ivar_layout : t -> uint8_t const_ptr -> void =
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

    external get_name : t -> sel =
      "Method_get_name"
    external get_implementation : t -> imp =
      "Method_get_implementation"
    external get_type_encoding : t -> TypeEnc.t =
      "Method_get_type_encoding"

    external get_number_of_arguments : t -> int =
      "Method_get_number_of_arguments"
    external copy_return_type : t -> char ptr =
      "Method_copy_return_type"
    external copy_argument_type : t -> int -> char ptr =
      "Method_copy_argument_type"
    (* external get_return_type : t -> char ptr -> size_t -> unit *)
    (* external get_argument_type : t -> int -> char ptr -> size_t -> unit *)
    external get_description : t -> description ptr =
      "Method_get_description"

    external set_implementation : t -> imp -> imp =
      "Method_set_implementation"
    external exchange_implementations : t -> t -> unit =
      "Method_exchange_implementations"
  end

module Ivar =
  struct
    type t = ivar

    external get_name : t -> cstr =
      "Ivar_get_name"
    external get_type_encoding : t -> TypeEnc.t =
      "Ivar_get_type_encoding"
    external get_offset : t -> ptrdiff_t =
      "Ivar_get_offset"
  end

module Property =
  struct
    type t = objc_property_t
    type attribute = objc_property_attribute_t

    external get_name : t -> cstr =
      "Property_get_name"
    external get_attributes : t -> cstr =
      "Property_get_attributes"
    external copy_attribute_list : t -> int ref -> attribute ptr (* 10.7 *) =
      "Property_copy_attribute_list"
    external copy_attribute_value : t -> cstr -> char ptr (* 10.7 *) =
      "Property_copy_attribute_value"
  end

module Protocol =
  struct
    (* type t = protcol *)
    type p = protocol ptr

    external conforms_to_protocol : p -> p -> bool =
      "Protocol_conforms_to_protocol"
    external is_equal : p -> p -> bool =
      "Protocol_is_equal"
    external get_name : p -> cstr =
      "Protocol_get_name"
    external get_method_description : p -> sel -> req:bool -> inst:bool -> Method.description =
      "Protocol_get_method_description"
    external copy_method_description_list : p -> req:bool -> inst:bool -> int ref -> Method.description ptr =
      "Protocol_copy_method_description_list"
    external get_property : p -> cstr -> req:bool -> inst:bool -> Property.t =
      "Protocol_get_property"
    external copy_property_list : p -> int ref -> Property.t ptr =
      "Protocol_copy_property_list"
    external copy_protocol_list : p -> int ref -> p ptr =
      "Protocol_copy_protocol_list"

    external add_method_description : p -> sel -> TypeEnc.t -> req:bool -> inst:bool -> unit (* 10.7 *) =
      "Protocol_add_method_description"
    external add_protocol : p -> p -> unit (* 10.7 *) =
      "Protocol_add_protocol"
    external add_property : p -> cstr -> Property.attribute ptr -> int -> req:bool -> inst:bool -> void (* 10.7 *) =
      "Protocol_add_property__bytecode"
      "Protocol_add_property__native"
  end

module Sel =
  struct
    type t = sel

    external get_name : t -> cstr =
      "Sel_get_name"
    external get_uid : cstr -> sel =
      "Sel_get_uid"
    external register_name : cstr -> sel =
      "Sel_register_name"
    external is_equal : sel -> sel -> bool =
      "Sel_is_equal"
  end
    
module Object =
  struct
    type t = id

    external copy : t -> size_t -> t =
      "Object_copy"
    external dispose : t -> t =
      "Object_dispose"

    external get_class : t -> Class.t =
      "Object_get_class"
    external set_class : t -> Class.t -> Class.t =
      "Object_set_class"

    external get_class_name : t -> cstr =
      "Object_get_class_name"
    external get_indexed_ivars : t -> void ptr =
      "Object_get_indexed_ivars"

    external get_ivar : t -> Ivar.t -> t =
      "Object_get_ivar"
    external set_ivar : t -> Ivar.t -> t -> unit =
      "Object_set_ivar"

    external set_instance_variable : t -> cstr -> void ptr -> Ivar.t =
      "Object_set_instance_variable"
    external get_instance_variable : t -> cstr -> void ptr ref -> Ivar.t =
      "Object_get_instance_variable"
  end

external construct_instance : Class.t -> void ptr -> id =
  "ObjC_construct_instance"
external destruct_instance : id -> void ptr =
  "ObjC_destruct_instance"

external allocate_class_pair : Class.t -> cstr -> size_t -> Class.t =
  "ObjC_allocate_class_pair"
external register_class_pair : Class.t -> unit =
  "ObjC_register_class_pair"
external duplicate_class : Class.t -> cstr -> size_t -> Class.t =
  "ObjC_duplicate_class"
external dispose_class_pair : Class.t -> unit =
  "ObjC_dispose_class_pair"

external copy_image_names : int ref -> cstr ptr =
  "ObjC_copy_image_names"
external copy_class_names_for_image : cstr -> int ref -> cstr ptr =
  "ObjC_copy_class_names_for_image"
    
external allocate_protocol : cstr -> Protocol.p (* 10.7 *) =
  "ObjC_allocate_protocol"
external register_protocol : Protocol.p -> unit (* 10.7 *) =
  "ObjC_register_protocol"

(* external objc_enumeration_mutation : id -> unit *)
(* external objc_set_enumeration_mutation_handler : (id -> unit) -> unit *)

(* external objc_set_forward_handler : void ptr -> void ptr -> unit *)

(* external imp_implementation_with_block : void ptr -> imp *)
(* external imp_get_block : imp -> void ptr *)
(* external imp_remove_block : imp -> bool *)

type association_policy = Assign | Retain_Nonatomic | Copy_Nonatomic | Retain | Copy

external set_associated_object : id -> cstr -> id -> association_policy -> void =
  "ObjC_set_associated_object"
external get_associated_object : id -> cstr -> id =
  "ObjC_get_associated_object"
external remove_associated_objects : id -> unit =
  "ObjC_remove_associated_objects"
    
external get_class : cstr -> id =
  "ObjC_get_class"
external get_meta_class : cstr -> id =
  "ObjC_get_meta_class"
external look_up_class : cstr -> id =
  "ObjC_look_up_class"
external get_required_class : cstr -> id =
  "ObjC_get_required_class"
external get_future_class : cstr -> id =
  "ObjC_get_future_class"
external set_future_class : Class.t -> cstr -> unit =
  "ObjC_set_future_class"
external get_class_list : Class.t ptr -> int -> int =
  "ObjC_get_class_list"
external copy_class_list : int ref -> Class.t ptr =
  "ObjC_copy_class_list"
external get_protocol : cstr -> Protocol.p =
  "ObjC_get_protocol"
external copy_protocol_list : int ref -> Protocol.p ptr =
  "ObjC_copy_protocol_list"
