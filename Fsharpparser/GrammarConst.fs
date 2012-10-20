module GrammarConst
open Attributes
//A.1.4.3     Keywords
(*for generating these the following awk is useful  awk '{printf("|[<Prefixs(\"%s\")>] %s\n" ,$0,$0)}' *)

type abstract_w = 
    |[<Prefixs("abstract")>] Abstract
type and_w =
    |[<Prefixs("and")>] And
type as_w = 
    |[<Prefixs("as")>] As
    |[<Prefixs("assert")>] Assert
    |[<Prefixs("base")>] Base
type begin_w = 
    |[<Prefixs("begin")>] Begin
    |[<Prefixs("class")>] Class
    |[<Prefixs("default")>] Default
type delegate_w = 
    |[<Prefixs("delegate")>] Delegate
    |[<Prefixs("do")>] Do
    |[<Prefixs("done")>] Done
    |[<Prefixs("downcast")>] Downcast
    |[<Prefixs("downto")>] Downto
    |[<Prefixs("elif")>] Elif
    |[<Prefixs("else")>] Else
type end_w = 
    |[<Prefixs("end")>] End
    |[<Prefixs("exception")>] Exception
    |[<Prefixs("extern")>] Extern
type false_w =
    |[<Prefixs("false")>] False
    |[<Prefixs("finally")>] Finally
    |[<Prefixs("for")>] For
    |[<Prefixs("fun")>] Fun
    |[<Prefixs("function")>] Function
    |[<Prefixs("global")>] Global
    |[<Prefixs("if")>] If
    |[<Prefixs("in")>] In
    |[<Prefixs("inherit")>] Inherit
type inline_w =
    |[<Prefixs("inline")>] Inline
type interface_w = 
    |[<Prefixs("interface")>] Interface
type internal_w = 
    |[<Prefixs("internal")>] Internal
type lazy_w = 
    |[<Prefixs("lazy")>] Lazy
type let_w =
    |[<Prefixs("let")>] Let
type match_w = 
    |[<Prefixs("match")>] Match
    |[<Prefixs("member")>] Member
type module_w =
    |[<Prefixs("module")>] Module
type mutable_w = 
    |[<Prefixs("mutable")>] Mutable
type namespace_w =
    |[<Prefixs("namespace")>] Namespace
type new_w = 
    |[<Prefixs("new")>] New
type null_w =
    |[<Prefixs("null")>] Null
type of_w =
    |[<Prefixs("of")>] Of
type open_w =
    |[<Prefixs("open")>] Open
type or_w = 
    |[<Prefixs("or")>] Or
    |[<Prefixs("override")>] Override
type private_w =
    |[<Prefixs("private")>] Private
type public_w = 
    |[<Prefixs("public")>] Public
type rec_w = 
    |[<Prefixs("rec")>] Rec
    |[<Prefixs("return")>] Return
    |[<Prefixs("sig")>] Sig
    |[<Prefixs("static")>] Static
type struct_w =
    |[<Prefixs("struct")>] Struct
    |[<Prefixs("then")>] Then
    |[<Prefixs("to")>] To
type true_w =
    |[<Prefixs("true")>] True
    |[<Prefixs("try")>] Try
type type_w =
    |[<Prefixs("type")>] Type
    |[<Prefixs("upcast")>] Upcast
    |[<Prefixs("use")>] Use
    |[<Prefixs("val")>] Val
    |[<Prefixs("void")>] Void
type when_w =
    |[<Prefixs("when")>] When
type while_w = 
    |[<Prefixs("while")>] While
type with_w = 
    |[<Prefixs("with")>] With
type yield_w = 
    |[<Prefixs("yield")>] Yield

//these are not in the spec - but add in anyway
type set_w = 
    |[<Prefixs("set")>] Set
type get_w = 
    |[<Prefixs("get")>] Get
type not_w = 
    |[<Prefixs("not")>] Not
type unit_w = 
    |[<Prefixs("unit")>] Unit
type enum_w = 
    |[<Prefixs("enum")>] Enum
type unmanaged_w = 
    |[<Prefixs("unmanaged")>] Unmanaged
type assembly_w = 
    |[<Prefixs("assembly")>] Unmanaged
type return_w = 
    |[<Prefixs("return")>] Unmanaged
type field_w = 
    |[<Prefixs("field")>] Unmanaged
type property_w = 
    |[<Prefixs("property")>] Unmanaged
type param_w = 
    |[<Prefixs("param")>] Unmanaged
type constructor_w = 
    |[<Prefixs("constructor")>] Unmanaged
type event_w = 
    |[<Prefixs("event")>] Unmanaged


type reserved_ident_keyword =
    |[<Prefixs("atomic")>] Atomic
    |[<Prefixs("break")>] Break
    |[<Prefixs("checked")>] Checked
    |[<Prefixs("component")>] Component
    |[<Prefixs("const")>] Const
    |[<Prefixs("constraint")>] Constraint
    |[<Prefixs("constructor")>] Constructor
    |[<Prefixs("continue")>] Continue
    |[<Prefixs("eager")>] Eager
    |[<Prefixs("fixed")>] Fixed
    |[<Prefixs("fori")>] Fori
    |[<Prefixs("functor")>] Functor
    |[<Prefixs("include")>] Include
    |[<Prefixs("measure")>] Measure
    |[<Prefixs("method")>] Method
    |[<Prefixs("mixin")>] Mixin
    |[<Prefixs("object")>] Object
    |[<Prefixs("parallel")>] Parallel
    |[<Prefixs("params")>] Params
    |[<Prefixs("process")>] Process
    |[<Prefixs("protected")>] Protected
    |[<Prefixs("pure")>] Pure
    |[<Prefixs("recursive")>] Recursive
    |[<Prefixs("sealed")>] Sealed
    |[<Prefixs("tailcall")>] Tailcall
    |[<Prefixs("trait")>] Trait
    |[<Prefixs("virtual")>] Virtual
    |[<Prefixs("volatile")>] Volatile

// 
//
//reserved-ident-formats :
//
//      ident-text ( '!' | '#')
//
//A.1.4.4     Symbolic Keywords
//
type equals = |[<Prefixc('=')>]Dummy
type open_brack = |[<Prefixc('(')>]Dummy
type open_square = |[<Prefixc('[')>]Dummy
type open_curly = |[<Prefixc('{')>]Dummy
type close_brack = |[<Prefixc(')')>]Dummy
type close_square = |[<Prefixc(']')>]Dummy
type close_curly = |[<Prefixc('}')>]Dummy
type star = |[<Prefixc('*')>]Dummy
type pipe = |[<Prefixc('|')>]Dummy
type underscore = |[<Prefixc('_')>]Dummy
type question = |[<Prefixc('?')>]Dummy
type lessthan = |[<Prefixc('<')>]Dummy
type dash = |[<Prefixc('-')>]Dummy
type bang = |[<Prefixc( '!')>]Dummy
type percent = |[<Prefixc( '%')>]Dummy
type amp = |[<Prefixc( '&')>]Dummy
type plus = |[<Prefixc( '+')>]Dummy
type dot = |[<Prefixc( '.')>]Dummy
type divslash = |[<Prefixc('/')>]Dummy
type escapeslash = |[<Prefixc('\\')>]Dummy
type at = |[<Prefixc( '@')>]Dummy
type caret = |[<Prefixc( '^')>]Dummy
type tilde = |[<Prefixc( '~')>]Dummy
type greaterthan = |[<Prefixc( '>')>]Dummy
type comma = |[<Prefixc( ',')>]Dummy
type colon = |[<Prefixc( ':')>]Dummy
type semicolon = |[<Prefixc( ';')>]Dummy
type quote = |[<Prefixc( ''')>]Dummy
type dquote = |[<Prefixc( '"')>]Dummy
type hash = |[<Prefixc( '#')>]Dummy


type doubledot = |[<Prefixs("..")>]Dummy
type dcop = |[<Prefixs(":>")>]Dummy
type goesto = |[<Prefixs("->")>]Dummy
type cons = |[<Prefixs("::")>]Dummy
type colonquest = |[<Prefixs(":?")>]Dummy
type plusdot = |[<Prefixs("+.")>]Dummy
type minusdot = |[<Prefixs("-.")>]Dummy
type ampamp = |[<Prefixs("&&")>]Dummy
//symbolic-keyword : one of
//
//      let! use! do! yield! return!
//
//      | -> <- . : ( ) [ ] [< >] [| |] { }
//
//      ' # :?> :? :> .. :: := ;; ; =
//
//      _ ? ?? (*) <@ @> <@@ @@>
//
// 
//
//reserved-symbolic-sequence :
//
//      ~ `
type onechar = |[<Prefixc('1')>]Dummy