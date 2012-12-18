module Fn : ZimtAST.FN

module MakeStruct
  (E : ZimtAST.ENV with type t = ZimtAST.deftype)
  (N : ZimtAST.NAMED)
  : ZimtAST.STRUCT

module MakeEnum
  (E : ZimtAST.ENV with type t = ZimtAST.defvalue)
  (N : ZimtAST.NAMED)
  : ZimtAST.ENUM

module MakeModule
  (D : ZimtAST.MODULE_DESC)
  : ZimtAST.MODULE
