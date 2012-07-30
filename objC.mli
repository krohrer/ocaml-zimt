exception IndexOutOfBounds
exception IsNil

type id
type class'
type property
type attributes
type sel
type imp
type ivar
type protocol
type type' = string
type types = string
type method'
type method_description = sel * types
type name = string
type size = int
type ivar_layout = string
type weak_ivar_layout = string
type version = int
type c_ptrdiff = int
type c_bytes
type c_pointer
type policy = [ `ASSIGN | `RETAIN_NONATOMIC | `COPY_NONATOMIC | `RETAIN | `COPY]

(* Working with Classes *)
external class_addIvar : class' -> name -> size -> alignment:int -> types -> bool = 
   "objc_class_addIvar"
(*
external class_addMethod : class' -> sel -> imp -> types -> bool =
   "objc_class_addMethod"
external class_addProtocol : class' -> protocol -> bool =
   "objc_class_addProtocol"
external class_conformsToProtocol : class' -> protocol -> bool =
   "objc_class_conformsToProtocol"
external class_copyIvarList : class' -> ivar array =
   "objc_class_copyIvarList"
external class_copyMethodList : class' -> method' array =
   "objc_class_copyMethodList"
external class_copyPropertyList : class' -> property array =
   "objc_class_copyPropertyList"
external class_copyProtocolList : class' -> protocol array =
   "objc_class_copyProtocolList"
external class_createInstance : class' -> extra:size -> id =
   "objc_class_createInstance"
external class_getClassMethod : class' -> sel -> method' =
   "objc_class_getClassMethod"
external class_getClassVariable : class' -> name -> ivar =
   "objc_class_getClassVariable"
external class_getInstanceMethod : class' -> method' =
   "objc_class_getInstanceMethod"
external class_getInstanceSize : class' -> size =
   "objc_class_getInstanceSize"
external class_getInstanceVariable : class' -> name -> ivar =
   "objc_class_getInstanceVariable"
external class_getIvarLayout : class' -> ivar_layout =
   "objc_class_getIvarLayout"
external class_getMethodImplementation : class' -> sel -> imp =
   "objc_class_getMethodImplementation"
external class_getMethodImplementation_stret : class' -> sel -> imp =
   "objc_class_getMethodImplementation_stret"
external class_getName : class' -> string =
   "objc_class_getName"
external class_getProperty : class' -> string -> property =
   "objc_class_getProperty"
external class_getSuperclass : class' -> class' =
   "objc_class_getSuperclass"
external class_getVersion : class' -> version =
   "objc_class_getVersion"
external class_getWeakIvarLayout : class' -> weak_ivar_layout  =
   "objc_class_getWeakIvarLayout"
external class_isMetaClass : class' -> bool =
   "objc_class_isMetaClass"
external class_replaceMethod : class' -> sel -> imp -> types -> imp =
   "objc_class_replaceMethod"
external class_respondsToSelector : class' -> sel -> bool =
   "objc_class_respondsToSelector"
external class_setIvarLayout : class' -> ivar_layout -> unit =
   "objc_class_setIvarLayout"
external class_setSuperclass : class' -> class' -> unit =
   "objc_class_setSuperclass"
external class_setVersion : class' -> version -> unit =
   "objc_class_setVersion"
external class_setWeakIvarLayout : class' -> weak_ivar_layout -> unit =
   "objc_class_setWeakIvarLayout"
(* external objc_getFutureClass : class' -> name -> class' *)
(* external objc_setFutureClass : class' -> name -> unit *)

(* Adding Classes *)
external objc_allocateClassPair : super:class' -> name -> extra:size -> class' =
   "objc_objc_allocateClassPair"
external objc_registerClassPair : class' -> unit =
   "objc_objc_registerClassPair"
(* external objc_duplicateClass : 'a *)

(* Instantiating Classes *)
external class_createInstance : class' -> extra:size -> id =
   "objc_class_createInstance"

(* Working with Instances *)
external object_copy : id -> size -> id =
   "objc_object_copy"
external object_dispose : id -> unit =
   "objc_object_dispose"
external object_setInstanceVariable : id -> name -> c_pointer -> ivar =
   "objc_object_setInstanceVariable"
external object_getInstanceVariable : id -> name -> ivar * id =
   "objc_object_getInstanceVariable"
external object_getIndexedIvars : id -> c_pointer =
   "objc_object_getIndexedIvars"
external object_getIvar : id -> ivar -> id =
   "objc_object_getIvar"
external object_setIvar : id -> ivar -> id -> unit =
   "objc_object_setIvar"
external object_getClassName : id -> name =
   "objc_object_getClassName"
external object_getClass : id -> class' =
   "objc_object_getClass"
external object_setClass : id -> class' -> class' =
   "objc_object_setClass"

(* Obtaining Class Definitions *)
external objc_getClassList : unit -> class' array =
   "objc_objc_getClassList"
external objc_lookUpClass : name -> id option =
   "objc_objc_lookUpClass"
external objc_getClass : id -> class' =
   "objc_objc_getClass"
external objc_getRequiredClass : name -> id =
   "objc_objc_getRequiredClass"
external objc_getMetaClass : name -> id =
   "objc_objc_getMetaClass"

(* Working with Instance Variables *)
external ivar_getName : ivar -> name =
   "objc_ivar_getName"
external ivar_getTypeEncoding : ivar -> types =
   "objc_ivar_getTypeEncoding"
external ivar_getOffset : ivar -> c_ptrdiff =
   "objc_ivar_getOffset"

(* Associative References *)
external objc_setAssociatedObject : id -> c_pointer -> id -> policy -> unit =
   "objc_objc_setAssociatedObject"
external objc_getAssociatedObject : id -> c_pointer -> id =
   "objc_objc_getAssociatedObject"
external objc_removeAssociatedObjects : id -> unit =
   "objc_objc_removeAssociatedObjects"

(* Sending Messages *)
external objc_msgSend : id -> sel -> id array -> id =
   "objc_objc_msgSend"
external objc_msgSend_fpret : id -> sel -> id array -> float =
   "objc_objc_msgSend_fpret"
external objc_msgSend_stret : id -> sel -> id array -> c_bytes =
   "objc_objc_msgSend_stret"
external objc_msgSendSuper : id -> sel -> id array -> id =
   "objc_objc_msgSendSuper"
external objc_msgSendSuper_stret : id -> sel -> id array -> c_bytes =
   "objc_objc_msgSendSuper_stret"

(* Working with Methods *)
external method_getName : method' -> sel =
   "objc_method_getName"
external method_getImplementation : method' -> imp =
   "objc_method_getImplementation"
external method_getTypeEncoding : method' -> types =
   "objc_method_getTypeEncoding"
external method_copyReturnType : method' -> type' =
   "objc_method_copyReturnType"
external method_copyArgumentType : method' -> int -> type' =
   "objc_method_copyArgumentType"
external method_getReturnType : method' -> type' =
   "objc_method_getReturnType"
external method_getNumberOfArguments : method' -> size =
   "objc_method_getNumberOfArguments"
external method_getArgumentType : method' -> int -> type' =
   "objc_method_getArgumentType"
external method_setImplementation : method' -> imp -> unit =
   "objc_method_setImplementation"
external method_exchangeImplementations : method' -> method' -> unit =
   "objc_method_exchangeImplementations"

(* Working with Selectors *)
external sel_getName : sel -> name =
   "objc_sel_getName"
external sel_registerName : name -> sel =
   "objc_sel_registerName"
external sel_getUid : name -> sel =
   "objc_sel_getUid"
external sel_isEqual : sel -> sel -> bool =
   "objc_sel_isEqual"

(* Working with Protocols *)
external objc_getProtocol : name -> protocol =
   "objc_objc_getProtocol"
external objc_copyProtocolList : unit -> protocol array =
   "objc_objc_copyProtocolList"
external protocol_getName : protocol -> name =
   "objc_protocol_getName"
external protocol_isEqual : protocol -> protocol -> bool =
   "objc_protocol_isEqual"
external protocol_copyMethodDescriptionList : protocol -> required:bool -> instance:bool -> method_description array =
   "objc_protocol_copyMethodDescriptionList"
external protocol_getMethodDescription : protocol -> sel -> required:bool -> instance:bool -> method_description =
   "objc_protocol_getMethodDescription"
external protocol_copyPropertyList : protocol -> property array =
   "objc_protocol_copyPropertyList"
external protocol_getProperty : protocol -> name -> required:bool -> instance:bool -> property =
   "objc_protocol_getProperty"
external protocol_copyProtocolList : protocol -> protocol array =
   "objc_protocol_copyProtocolList"
external protocol_conformsToProtocol : protocol -> protocol -> bool =
   "objc_protocol_conformsToProtocol"

(* Working with Properties *)
external property_getName : property -> name =
   "objc_property_getName"
external property_getAttributes : property -> attributes =
   "objc_property_getAttributes"
*)
