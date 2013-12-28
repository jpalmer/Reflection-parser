module VMGrammar
open Attributes
open JackAsm
type segment = |[<Prefixs("constant")>]C 

type push_instruc = |P  of whitespace * segment * Option<whitespace * int_literal>
type line = 
    |[<Prefixs("push")>]Push of push_instruc
    |[<Prefixs("add")>]Add
    |[<Prefixs("eq")>]Eq
    |[<Prefixs("lt")>]Lt
    |[<Prefixs("gt")>]Gt
    |[<Prefixs("sub")>]Sub
    |[<Prefixs("neg")>]Neg
    |[<Prefixs("and")>]And
    |[<Prefixs("or")>]Or
    |[<Prefixs("not")>]Not
type main = |L of (Option<line> * Option<whitespace> * Option<end_of_line_comment> * newline) list
