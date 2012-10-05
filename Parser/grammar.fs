module grammar 
open Attributes
open System.Globalization
//A.1 Lexical Grammar
//
//A.1.1      Whitespace
//
type wschar = |[<Prefixc(' ')>] Ws
type whiteSpace = |Wspace of Plus<wschar>
type newline = |[<Prefixs("\r\n")>] Nl //should add support for linux newlines
type whitespace_or_newline =
    |WhiteSpace of whiteSpace
    |Newline of newline

//A.1.2      Comments
//
type block_comment_start = |[<Prefixs("(*")>] Bcs
type block_comment_end = |[<Prefixs("*)")>] Bce
type inside_End_of_line_comment = |[<NotPrefixc([|'\n';'\r'|])>] ELcc
type end_of_line_comment = |[<Prefixs(@"//")>] Elc of inside_End_of_line_comment list

// 
//A.1.4      Identifiers and Keywords
//
//A.1.4.1     Identifiers
//
type digit_char = |[<GrabPrefixClass[|UnicodeCategory.DecimalDigitNumber|]>] DC of char
type letter_char = |[<GrabPrefixClass[|UnicodeCategory.UppercaseLetter;UnicodeCategory.LowercaseLetter;UnicodeCategory.TitlecaseLetter;UnicodeCategory.ModifierLetter;
                        UnicodeCategory.OtherLetter;UnicodeCategory.LetterNumber|]>] LC of char
type connecting_char = |[<GrabPrefixClass[|UnicodeCategory.ConnectorPunctuation|]>] PC of char
type combining_char = |[<GrabPrefixClass[|UnicodeCategory.NonSpacingMark;UnicodeCategory.SpacingCombiningMark |]>] CC of char
type formatting_char =  |[<GrabPrefixClass[|UnicodeCategory.Format|]>] PC of char
type ident_start_char =
    |LC of letter_char |[<Prefixc('_')>] Underscore
type ident_char = //the spec has an entry for `_` here but it is not needed - it is included in Connecting_char
    |LC of letter_char
    |DC of digit_char
    |CC of connecting_char
    |CoC of combining_char
    |FC of formatting_char
    |[<Prefixc(''')>]Quote 
type ident_text = |IDent_text of ident_start_char * (ident_char list)
//
//A.1.3      Conditional Compilation
//
type else_directive = |[<Prefixs("#else")>] Elsed
type endif_directive = |[<Prefixs("#endif")>] Endifd
type if_directive = |[<Prefixs("#if")>] Ifd of whiteSpace * ident_text

type ident = |IT of ident_text //the spec has some weird stuff here - but this is what the source does
type _dotchar = |[<Prefixc('.')>]Dotchar
type _ident_dot = |Ident_dot of ident * _dotchar
type long_ident = |LI of ident * ((_dotchar * ident) list)
//
//
//long-ident-or-op : 
//
//      long-ident '.' ident-or-op
//
//      ident-or-op
//
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
type slash = |[<Prefixc( '/')>]Dummy
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
//
//A.1.5      Strings and Characters
//
//escape-char :  '\' ["\'ntbr]
//
// 
//
//non-escape-chars :  '\' [^"\'ntbr]
//
// 
//
//simple-char-char : any char except
//
//      '\n' '\t' '\r' '\b' ' \ "
//
// 
//
and uchar = |[<Prefixc('u')>] Dummy 
and Uchar = |[<Prefixc('U')>] Dummy
and Bchar = |[<Prefixc('B')>] Dummy
and unicodegraph_short = |Ugraphshort of slash * uchar *hexdigit*hexdigit *hexdigit*hexdigit 
and unicodegraph_long =  |Ugraphlong of  slash * Uchar * hexdigit*hexdigit * hexdigit*hexdigit* hexdigit*hexdigit* hexdigit*hexdigit 
//FIXME: not in spec
and trigraph = TG of slash * digit_char * digit_char * digit_char
and char_char = |SCC of simple_char_char |EC of escape_char |Trgr of trigraph |Unicode_short of unicodegraph_short
and string_char =
|SSC of simple_string_char |EC of escape_char   |NEC of non_escape_char |Trgr of trigraph
|UCG_short of unicodegraph_short                |UCG_long of unicodegraph_long
|Newline of newline

//
//string-elem :
//
//      string-char
//
//      '\' newline whitespace* string-elem
//
// 
//
and char_ = |Char of quote * char_char * quote 
//
// 
//
and string_ = |String of dquote * List<string_char> * dquote 
//
// 
//
and verbatim_string_char =
|SSC of simple_string_char |NEC of non_escape_chars
|NL of newline             |Slash of slash
|DDquote of dquote * dquote
and verbatim_string = VS of at * dquote * List<verbatim_string_char> * dquote 
and bytechar = |BC of quote * simple_or_escape_char * quote * Bchar
//bytearray          :  " string-char* "B
//
// 
//
//verbatim-bytearray : @" verbatim-string-char* "B
//
// 
//
//simple-or-escape-char :
//
//      escape-char
//
//      simple-char
//
// 
//
//simple-char : any char except
//
//      newline, return, tab, backspace,',\,"
//
//A.1.6      Numeric Literals
//
and digit  = |[<GrabPrefixClass([|System.Globalization.UnicodeCategory.DecimalDigitNumber|])>] D
//
// 
//
and hexchar = 
|[<Prefixc('A')>] A |[<Prefixc('B')>] B |[<Prefixc('C')>] C |[<Prefixc('D')>] D |[<Prefixc('E')>] E |[<Prefixc('F')>] F
|[<Prefixc('a')>] Aa|[<Prefixc('b')>] Bb|[<Prefixc('c')>] Cc|[<Prefixc('d')>] Dd|[<Prefixc('e')>] Ee|[<Prefixc('f')>] Ff
and hexdigit =
|D of digit
|H of hexchar
and octaldigit =
|[<Prefixc('0')>] D0 |[<Prefixc('1')>] D1 |[<Prefixc('2')>] D2 |[<Prefixc('3')>] D3 
|[<Prefixc('4')>] D4 |[<Prefixc('5')>] D5 |[<Prefixc('6')>] D6 |[<Prefixc('7')>] D7 
and bitdigit = |[<Prefixc('0')>] D0 |[<Prefixc('1')>] D1
//
// 
//
and int_ =  Plus<digit>
//
// 
//
and zerochar = |[<Prefixc('0')>] Dummy
and xXchar = |[<Prefixc('X')>] Dummy|[<Prefixc('x')>] Dummy2
and oOchar = |[<Prefixc('O')>] Dummy|[<Prefixc('o')>] Dummy2
and bBchar = |[<Prefixc('B')>] Dummy|[<Prefixc('b')>] Dummy2
and xint =
|Int of int
|Hexint of zerochar * xXchar * Plus<hexdigit>
|Octint of zerochar * xXchar * Plus<octaldigit>
|Binint of zerochar * xXchar * Plus<bitdigit>

and ychar =     |[<Prefixc('y')>]Dummy
and schar =     |[<Prefixc('s')>]Dummy
and lchar =     |[<Prefixc('l')>]Dummy
and Lchar =     |[<Prefixc('L')>]Dummy
and Fchar =     |[<Prefixc('F')>]Dummy
and fchar =     |[<Prefixc('f')>]Dummy
and uychar =    |[<Prefixs("uy")>]Dummy
and uschar =    |[<Prefixs("us")>]Dummy
and ULchar =    |[<Prefixs("UL")>]Dummy
and uLchar =    |[<Prefixs("uL")>]Dummy
and lfchar =    |[<Prefixs("lf")>]Dummy
and LFchar =    |[<Prefixs("LF")>]Dummy

and sbyte_ =    |Sbyte of   xint * ychar
and byte_  =    |Byte of    xint * uychar
and int16_ =    |Int16 of   xint * schar 
and uint16_ =   |Uint16 of  xint * uschar 
and int32_ =    |Int32 of   xint * lchar
and eEchar =  |[<Prefixc('E')>] Dummy|[<Prefixc('e')>] Dummy2
and pmchar =  |[<Prefixc('+')>] Plus|[<Prefixc('-')>] Minus
and bignumId = 
|[<Prefixc('Q')>] Dummy  |[<Prefixc('R')>] Dummy2 |[<Prefixc('Z')>] Dummy3
|[<Prefixc('I')>] Dummy4 |[<Prefixc('N')>] Dummy5 |[<Prefixc('G')>] Dummy6

//uint32 :
//
//      xint 'ul'
//
//      xint 'u'
//
// 
//
//nativeint : xint 'n'
//unativeint : xint 'un'
and int64_ =    |Int64 of   xint * Lchar
and uint64_ =   |Uint64 of  xint * ULchar 
                |Uint64_ of xint * uLchar
and ieee32 =    |Float of   float_ * Fchar
                |Float_ of  float_ * fchar
                |Fint of    xint * lfchar
// 
//
and ieee64 =    |Float of   float_
                |Fint of    xint * LFchar

and bignum = int_ * bignumId 
//
// 
//
//decimal : (float|int) [Mm]
//
// 
//
and float_ =
|Dec of Plus<digit> * dot * List<digit>
|Decl of Plus<digit> * Option<dot * List<digit>> * eEchar * Option<pmchar> * List<digit>

//reserved-literal-formats :
//
//      (xint | ieee32 | ieee64) ident-char+
//
// 
//
//A.1.7      Line Directives
//
//line-directive :
//
//      # int
//      # int string
//      # int verbatim-string
//
//      #line int
//      #line int string
//      #line int verbatim-string
//
//A.1.8      Identifier Replacements
//
//__SOURCE_DIRECTORY__
//
//__SOURCE_FILE__
//
//__LINE__
//
//A.1.9      Operators
//
//A.1.9.1     Operator Names
//
type ident_or_op =
|Id of ident
|ON of open_brack * op_name * close_brack
|Star of open_brack * star * close_brack
// 
//
and op_name =
|SO of symbolic_op
|RO of range_op_name
|AP of active_pattern_op_name
//
// 
//
and range_op_name =
|Dots of doubledot
|DDots of doubledot * doubledot
//
// 
//
and active_pattern_op_name =
|Without_ of pipe * ident * List<pipe*ident> * pipe
|With_    of pipe * ident * List<pipe*ident> * pipe * underscore * pipe
//
//A.1.9.2     Symbolic Operators
//
and first_op_char =  
|Bang of bang           |Percent of percent |Amp of amp                 |Star of star   |Plus of plus   |Dash of dash 
|Dot of dot             |Slash of slash     |At of at                   |Caret of caret |Pipe of pipe   |Tilde of tilde
|Lessthan of lessthan   |Equals of equals   |Greaterthan of greaterthan 
and op_char =
|FOC of first_op_char
|Question of question
and quote_op_left =
|[<Prefixs("<@@")>]Double 
|[<Prefixs("<@")>]Single 
and quote_op_right =
|[<Prefixs("@@>")>]Double 
|[<Prefixs("@>")>]Single 
//
and symbolic_op = 
|Q of question
|Qassign of question*lessthan*dash
|Oplist of first_op_char * List<op_char>
|Qopleft of quote_op_left
|Qopright of quote_op_right
//
//A.1.9.3     Infix and Prefix Operators
//
//The OP marker represents all symbolic-op tokens that begin with the indicated prefix, except for tokens that appear elsewhere in the table.
//
//infix-or-prefix-op : one of
//
//      +,  -, +., -., %, &, &&
//
// 
//
//prefix-op :
//
//      infix-or-prefix-op
//
//      ~ ~~ ~~~      (and any repetitions of ~)
//
//      !OP           (all tokens that begin with ! except !=)
//
// 
//
//infix-op :
//
//      infix-or-prefix-op
//
//      -OP +OP || <OP >OP = |OP &OP ^OP *OP /OP %OP !=
//
//                         (or any of these preceded by one or more ‘.’)
//
//      :=
//
//      ::
//
//      $
//
//      or
//
//      ?
//
//A.1.9.4     Constants
//
//not in spec but useful
and measure_encased = lessthan * measure_literal * greaterthan
//FIXME??? interesting - Decimal can't be without measure
and const_ = 
|Sbyte of sbyte_    |Int16 of int16_    |Int32 of int32_    |Int64 of int64_    |Byte of byte_      |Uint16 of uint16_ 
|Uint32 of uint32   |Int of int_        |Uint64 of uint64_  |Ieee32 of ieee32   |Ieee64 of ieee64   |Bignum of bignum
|Char of char_      |String of string_  |Verbatim_str of verbatim_string        |Bytestring of bytestring
|Bytechar of bytechar                   |False of false_w   |True of true_w     |Unit of open_brack * close_brack
|Sbytem of sbyte_ * measure_encased     |Int16m of int16_ * measure_encased     |Int32m of int32_ * measure_encased
|Ieee32m of ieee32 * measure_encased    |Ieee64m of ieee64 * measure_encased    |Decimalm of decimal * measure_encased
// 
//
//A.2 Syntactic Grammar
//
//In general, this syntax summary describes full syntax. By default, however, .fs, .fsi, .fsx, and .fsscript files support lightweight syntax, in which indentation replaces begin/end and done tokens. This appendix uses beginopt, endopt, and doneopt ­­to indicate that these tokens are omitted in lightweight syntax. Complete rules for lightweight syntax appear in §15.1.
//
//To disable lightweight syntax:
//
//#indent "off"
//
//When lightweight syntax is disabled, whitespace can include tab characters:
//
//whitespace : [ ' ' '\t' ]+
//
//A.2.1     Program Format
//
and implementation_file =
|NDL of Plus<namespace_decl_group>     
//|NM of named_module
//|AM of anonynmous_module
//
// 
//
//script-file : implementation-file
//
// 
//
//signature-file:
//
//      namespace-decl-group-signature ... namespace-decl-group-signature
//
//      anonynmous-module-signature
//
//      named-module-signature
//
// 
//
//named-module : module long-ident module-elems
//
// 
//
//anonymous-module : module-elems
//
// 
//
//named-module-signature : module long-ident module-signature-elements
//
// 
//
//anonymous-module-signature : module-signature-elements
//
// 
//
//script-fragment : module-elems
//
//A.2.1.1     Namespaces and Modules
//
and namespace_decl_group =
|NamespaceLocal of  namespace_w* long_ident * module_w * module_elems
//
//      namespace global module-elems
//
// 
//
//module-defn : attributesopt module accessopt ident = beginopt module-defn-body endop
//
// 
//
//module-defn-body : begin module-elemsopt end
//
// 
//
and module_elem =
    |MFVD of  module_function_or_value_defn
//
//      type-defns
//
//      exception-defn
//
//      module-defn
//
//      module-abbrev
//
//      import-decl
//
//      compiler-directive-decl
//
// 
//
and module_function_or_value_defn =
//
|LetF of      Option<attributes> * let_w * function_defn
//
//      attributesopt let value-defn
//
//      attributesopt let recopt function-or-value-defns
//
//      attributesopt do expr 
//
// 
//
//import-decl : open long-ident
//
// 
//
//module-abbrev : module ident = long-ident
//
// 
//
//compiler-directive-decl : # ident string ... string
//
// 
//
and module_elems = |ME of module_elem list 
and access =
|Private of private_w
|Internal of internal_w
|Public of public_w
//
//A.2.1.2     Namespace and Module Signatures
//
//namespace-decl-group-signature : namespace long-ident module-signature-elements
//
// 
//
//module-signature : module ident = beginopt module-signature-body endopt
//
// 
//
//module-signature-element :
//
//      val mutableopt curried-sig
//
//      val value-defn
//
//      type type-signatures
//
//      exception exception-signature
//
//      module-signature
//
//      module-abbrev
//
//      import-decl
//
// 
//
//module-signature-elements :
//
//      beginopt module-signature-element ... module-signature-element endopt
//
// 
//
//module-signature-body : begin module-signature-elements end
//
// 
//
//type-signature :
//
//      abbrev-type-signature
//
//      record-type-signature
//
//      union-type-signature
//
//      anon-type-signature
//
//      class-type-signature
//
//      struct-type-signature
//
//      interface-type-signature
//
//      enum-type-signature
//
//      delegate-type-signature
//
//      type-extension-signature
//
// 
//
//type-signatures : type-signature ... and ... type-signature
//
// 
//
//type-signature-element :
//
//      attributesopt accessopt new : uncurried-sig
//
//      attributesopt member accessopt member-sig   
//
//      attributesopt abstract accessopt member-sig
//
//      attributesopt override member-sig  
//
//      attributesopt default member-sig               
//
//      attributesopt static member accessopt member-sig
//
//      interface type
//
// 
//
//abbrev-type-signature : type-name '=' type
//
// 
//
//union-type-signature : type-name '=' union-type-cases type-extension-elements-signatureopt
//
// 
//
//record-type-signature :
//
//      type-name '=' '{' record-fields '}' type-extension-elements-signatureopt
//
// 
//
//anon-type-signature : type-name '=' begin type-elements-signature end
//
// 
//
//class-type-signature : type-name '=' class type-elements-signature end
//
// 
//
//struct-type-signature : type-name '=' struct type-elements-signature end
//
// 
//
//interface-type-signature : type-name '=' interface type-elements-signature end
//
// 
//
//enum-type-signature : type-name '=' enum-type-cases
//
// 
//
//delegate-type-signature :  type-name '=' delegate-sig
//
// 
//
//type-extension-signature : type-name type-extension-elements-signature
//
// 
//
//type-extension-elements-signature : with type-elements-signature end
//
//A.2.2      Types and Type Constraints
//
and type_ =
|Arrow of type_ * goesto * type_
|Tuple of type_ * List<star * type_>
|LIGB of long_ident * lessthan * greaterthan //TODO: should this have an _ in it??
|TLI of type_ * long_ident
|TA of type_ * open_square * List<comma> * close_square
|TL of type_ * lazy_w
|TTydefs of type_ * typar_defns
|DC of typar * dcop * type_
|AT of atomic_type
and types = |Tlist of type_ * List<comma * type_> 
//some things moved into here
and atomic_type = //TODO: the spec has unbalanced brackets here
|Hasht of hash * type_
|Typar of typar
|Brack of open_brack  * type_ * close_brack
|LI of long_ident
|LIG of long_ident * lessthan * types * greaterthan
and typar =
|Underscore of underscore
|[<Prefixc(''')>]PrimedId of ident
|[<Prefixc('^')>]CaretId of ident
and constraint_ =
|Downcast of typar * dcop * type_
|Null of typar * colon * null_w
|ST of static_typars * colon * open_brack*member_sig*close_brack
|New of typar * colon * open_brack * new_w * colon * unit_w * goesto * typar * close_brack
|Struct of typar * colon * struct_w
|NStruct of typar * colon * not_w * struct_w
|Enum of typar * colon * enum_w * lessthan * type_w * greaterthan
|Unman of typar * colon * unmanaged_w
|Delegate of typar * colon * delegate_w * lessthan * type_ * comma * type_ * greaterthan
and typar_defn = |TD of Option<attributes> * typar  
and typar_defns = |TDs of lessthan * typar_defn * List<comma * typar_defn> * Option<typar_constraints> * greaterthan  
and typar_constraints = |TC of when_w * constraint_ * List<and_w * constraint_> 
and static_typars =
|Ident  of caret * ident
|Multi of open_brack * caret * ident * List<or_w*caret*ident> * close_brack
// 
//
//A.2.2.1     Equality and Comparison Constraints
//
//typar : equality
//
//typar : comparison
//
//A.2.3      Expressions
//
//expr : 
//
//      const
//
//      ( expr )
//
//      begin expr end
//
//      long-ident-or-op
//
//      expr '.' long-ident-or-op
//
//      expr expr
//
//      expr(expr)
//
//      expr<types>
//
//      expr infix-op expr
//
//      prefix-op expr
//
//      expr.[expr]
//
//      expr.[slice-range]
//
//      expr.[slice-range, slice-range]
//
//      expr <- expr
//
//      expr , ... , expr
//
//      new type expr
//
//      { new base-call object-members interface-impls }
//
//      { field-initializers }
//
//      { expr with field-initializers }
//
//      [ expr ; ... ; expr ]
//
//      [| expr ; ... ; expr |]
//
//      expr { comp-or-range-expr }
//
//      [ comp-or-range-expr]
//
//      [| comp-or-range-expr |]
//
//      lazy expr
//
//      null   
//
//      expr : type
//
//      expr :> type
//
//      expr :? type
//
//      expr :?> type
//
//      upcast expr
//
//      downcast expr
//
//In the following four expression forms, the in token is optional if expr appears on a subsequent line and is aligned with the let token.
//
//      let function-defn in expr 
//
//      let value-defn in expr     
//
//      let rec function-or-value-defns in expr
//
//      use ident = expr in expr
//
// 
//
//      fun argument-pats -> expr
//
//      function rules
//
//      match expr with rules
//
//      try expr with rules
//
//      try expr finally expr
//
//      if expr then expr elif-branchesopt else-branchopt
//
//      while expr do expr doneopt
//
//      for ident = expr to expr do expr doneopt
//
//      for pat in expr-or-range-expr do expr doneopt
//
//      assert expr
//
//      <@ expr @>
//
//      <@@ expr @@>
//
//      %expr
//
//      %%expr
//
//      (static-typars : (member-sig) expr)
//
//      expr $app expr       // equivalent to "expr(expr)"
//
//      expr $sep expr       // equivalent to "expr; expr"
//
//      expr $tyapp < types > // equivalent to "expr<types>"
//
//      expr< >
//
//exprs : expr ',' ... ',' expr
//
// 
//
//expr-or-range-expr :
//
//      expr
//
//      range-expr
//
// 
//
//elif-branches : elif-branch ... elif-branch
//
// 
//
//elif-branch : elif expr then expr
//
// 
//
//else-branch : else expr
//
// 
//
//function-or-value-defn :
//
//    function-defn
//
//    value-defn
//
and function_defn =
|FD of Option<inline_w> * Option<access> * ident_or_op *  Option<typar_defns> * argument_pats * Option<return_type> * equals *  expr
//
//value-defn :
//
//    mutableopt accessopt pat typar-defnsopt return-typeopt = expr
//
// 
//
and return_type = |Ret of colon * type_
// 
//
//function-or-value-defns :
//
//      function-or-value-defn and ... and function-or-value-defn
//
// 
//
and argument_pats= Plus<atomic_pat> 
//
// 
//
//field-initializer : long-ident = expr
//
// 
//
//field- initializer s : field-
//
//initializer ; ... ; field-initializer
//
// 
//
and object_construction =
|TE of type_ * expr
|T of type_
//
// 
//
//base-call :
//
//      object-construction 
//
//      object-construction as ident
//
// 
//
//interface-impls : interface-impl ... interface-impl
//
// 
//
//interface-impl : interface type object-membersopt
//
// 
//
//object-members : with member-defns end
//
// 
//
//member-defns :  member-defn ... member-defn
//
//A.2.3.1     Computation and Range Expressions
//
//comp-or-range-expr :
//
//      comp-expr
//
//      short-comp-expr
//
//      range-expr
//
// 
//
//comp-expr :
//
//      let! pat = expr in comp-expr
//
//      do!  expr in comp-expr     
//
//      use! pat = expr in comp-expr
//
//      yield! expr
//
//      yield expr    
//
//      return! expr
//
//      return expr   
//
//      expr   
//
// 
//
//short-comp-expr : for pat in expr-or-range-expr -> expr
//
// 
//
//range-expr :
//
//      expr .. expr        
//
//      expr .. expr .. expr
//
// 
//
//slice-range :
//
//      expr.. 
//
//      ..expr 
//
//      expr..expr
//
//      '*'
//
//A.2.3.2     Computation Expressions
//
//expr { for ... }
//
//expr { let ... }
//
//expr { let! ... }
//
//expr { use ... }
//
//expr { while ... }
//
//expr { yield ... }
//
//expr { yield! ... }
//
//expr { try ... }
//
//expr { return ... }
//
//expr { return! ... }
//
//A.2.3.3     Sequence Expressions
//
//seq { comp-expr }
//
//seq { short-comp-expr }
//
//A.2.3.4     Range Expressions
//
//seq { e1 .. e2 } 
//
//seq { e1 .. e2 .. e3 }
//
//A.2.3.5     Copy and Update Record Expression
//
//      { expr with field-label1 = expr1 ; … ; field-labeln = exprn }
//
//A.2.3.6     Dynamic Operator Expressions
//
//expr ? ident              → (?) expr "ident"
//
//expr1 ? (expr2)            → (?) expr1 expr2
//
//expr1 ? ident <- expr2    → (?<-) expr1 "ident" expr2
//
//expr1 ? (expr2) <- expr3  → (?<-) expr1 expr2 expr3
//
//"ident" is a string literal containing the text of ident.
//
//A.2.3.7     AddressOf Operators
//
//&expr
//
//&&expr
//
//A.2.3.8     Lookup Expressions
//
//e1.[e2]                    → e1.get_Item(e2)
//
//e1.[e2, e3]                       → e1.get_Item(e2, e3)
//
//e1.[e2, e3, e4]                   → e1.get_Item(e2, e3, e4)
//
//e1.[e2, e3, e4, e5]               → e1.get_Item(e2, e3, e4, e5)
//
//e1.[e2] <- e3              → e1.set_Item(e2, e3)
//
//e1.[e2, e3] <- e4                 → e1.set_Item(e2, e3, e4)
//
//e1.[e2, e3, e4] <- e5             → e1.set_Item(e2, e3, e4, e5)
//
//e1.[e2, e3, e4, e5] <- e6         → e1.set_Item(e2, e3, e4, e5, e6)
//
//A.2.3.9     Shortcut Operator Expressions
//
//expr1 && expr2                    → if expr1 then expr2 else false
//
//expr1 || expr2                    → if expr1 then true else expr2
//
//A.2.3.10 Deterministic Disposal Expressions
//
//use ident = expr1 in expr2
//
//A.2.4      Patterns
//
//rule : pat pattern-guardopt -> expr
//
// 
//
//pattern-guard : when expr
//
// 
//
and pat = //some of these moved into atomic_pat
|As of pat * as_w * ident
|Or of pat * pipe * pat
|And of pat * amp * pat
|Cons of pat * cons * pat
|Type of pat * colon * type_
|Tuple of pat * List<comma*pat>
|DowncastGet of colonquest * atomic_type * as_w * ident
|Attr of attributes * pat
|Atomic_pat of atomic_pat
// 
//
and list_pat =
|Empty of open_square * close_square
|List of open_square * pat * List<semicolon*pat> * close_square
and array_pat =
|Empty of open_square * pipe*pipe *close_square
|List of open_square*pipe * pat * List<semicolon*pat> * pipe * close_square
and record_pat = |RP of open_curly * field_pat * List<semicolon*field_pat> * close_curly 
and atomic_pat =
|Const of const_
|Li of long_ident * Option<pat_param> * Option<pat>
|List_pat of list_pat
|Record_pat of record_pat
|Array_pat of array_pat
|Brack of open_brack * pat * close_brack
|Downcast of colonquest * atomic_type
|Null of null_w
|Underscore of underscore

//
//    pat      one of
//
//               const  long-ident  list-pat  record-pat  array-pat  (pat)
//
//            :? atomic-type 
//
//                       null  _ _
and field_pat = FP of long_ident * equals * pat 
//
//pat-param :
//
//      const
//
//      long-ident
//
//      [ pat-param ; ... ; pat-param ]
//
//      ( pat-param, ..., pat-param )
//
//      long-ident pat-param
//
//      pat-param : type
//
//      <@ expr @>
//
//      <@@ expr @@>
//
//      null
//
// 
//
//pats :  pat , ... , pat
//
// 
//
//field-pats : field-pat ; ... ; field-pat
//
// 
//
//rules : '|'opt rule '|' ... '|' rule
//
//A.2.5      Type Definitions
//
//type-defn : 
//
//      abbrev-type-defn
//
//      record-type-defn
//
//      union-type-defn
//
//      anon-type-defn
//
//      class-type-defn
//
//      struct-type-defn
//
//      interface-type-defn
//
//      enum-type-defn
//
//      delegate-type-defn
//
//      type-extension
//
// 
//
//type-name : attributesopt accessopt ident typar-defnsopt
//
// 
//
//abbrev-type-defn : type-name = type
//
// 
//
//union-type-defn : type-name '=' union-type-cases type-extension-elementsopt
//
// 
//
//union-type-cases : '|'opt union-type-case '|' ... '|' union-type-case    
//
// 
//
//union-type-case : attributesopt union-type-case-data
//
// 
//
//union-type-case-data :
//
//      ident                -- nullary union case
//
//      ident of type * ... * type  -- n-ary union case
//
//      ident : uncurried-sig -- n-ary union case
//
// 
//
//anon-type-defn :
//
//      type-name primary-constr-argsopt object-valopt '=' begin class-type-body end
//
// 
//
//record-type-defn : type-name = '{' record-fields '}' type-extension-elementsopt
//
// 
//
//record-fields : record-field ; ... ; record-field ;opt
//
// 
//
//record-field :  attributesopt mutableopt accessopt ident : type
//
// 
//
//class-type-defn :
//
//      type-name primary-constr-argsopt object-valopt '=' class class-type-body end
//
// 
//
//as-defn : as ident
//
// 
//
//class-type-body :
//
//      beginopt class-inherits-declopt class-function-or-value-defnsopt type-defn-elementsopt endopt
//
// 
//
//class-inherits-decl : inherit type expropt
//
// 
//
//class-function-or-value-defn :
//
//      attributesopt staticopt let recopt function-or-value-defns
//
//      attributesopt staticopt do expr
//
// 
//
//struct-type-defn :
//
//    type-name primary-constr-argsopt as-defnopt '=' struct struct-type-body end
//
// 
//
//struct-type-body : type-defn-elements
//
// 
//
//interface-type-defn : type-name '=' interface interface-type-body end
//
// 
//
//interface-type-body : type-defn-elements
//
// 
//
//exception-defn :
//
//      attributesopt exception union-type-case-data     
//
//      attributesopt exception ident = long-ident
//
// 
//
//enum-type-defn : type-name '=' enum-type-cases
//
// 
//
//enum-type-cases : '|'opt enum-type-case '|' ... '|' enum-type-case    
//
// 
//
//enum-type-case : ident '=' const
//
// 
//
//delegate-type-defn : type-name '=' delegate-sig
//
// 
//
//delegate-sig : delegate of uncurried-sig
//
// 
//
//type-extension : type-name type-extension-elements
//
// 
//
//type-extension-elements : with type-defn-elements end
//
// 
//
//type-defn-element :
//
//      member-defn
//
//      interface-impl
//
//      interface-signature
//
// 
//
//type-defn-elements : type-defn-element ... type-defn-element 
//
// 
//
//primary-constr-args : attributesopt accessopt (simple-pat, ... , simplepat)
//
//simple-pat :
//
//    | ident
//
//    | simple-pat : type
//
// 
//
// 
//
//additional-constr-defn :
//
//      attributesopt accessopt new pat as-defn = additional-constr-expr
//
// 
//
//additional-constr-expr :
//
//      stmt ';' additional-constr-expr
//
//      additional-constr-expr then expr
//
//      if expr then additional-constr-expr else additional-constr-expr
//
//      let val-decls in  additional-constr-expr
//
//      additional-constr-init-expr
//
// 
//
//additional-constr-init-expr :
//
//      '{' class-inherits-decl field-initializers '}'
//
//      new type expr
//
// 
//
//member-defn :
//
//      attributesopt staticopt member accessopt method-or-prop-defn
//
//      attributesopt abstract memberopt accessopt member-sig
//
//      attributesopt override accessopt method-or-prop-defn
//
//      attributesopt default accessopt method-or-prop-defn
//
//      attributesopt staticopt val mutableopt accessopt ident : type
//
//      additional-constr-defn
//
// 
//
//method-or-prop-defn :
//
//      identopt function-defn
//
//      identopt value-defn
//
//      identopt ident with function-or-value-defns
//
// 
//
and member_sig =
//
|NoGetSet of  ident *  Option<typar_defns> *colon *  curried_sig
|Get of  ident *  Option<typar_defns> *colon *  curried_sig * with_w * get_w
|Set of  ident *  Option<typar_defns> *colon*  curried_sig * with_w * set_w
|GetSet of  ident *  Option<typar_defns> *colon *  curried_sig * with_w * get_w * comma * set_w
|SetGet of  ident *  Option<typar_defns> *colon *  curried_sig * with_w * set_w * comma * get_w
// 
//
and curried_sig = args_spec * List<goesto * args_spec> * type_ 
//
// 
//
//uncurried-sig : args-spec -> type
and args_spec = arg_spec * List<star * arg_spec> 
and arg_spec  = Option<attributes> * Option<arg_name_spec> * type_
//
// 
//
and arg_name_spec = Option<question> * ident * colon
//
// 
//
//interface-spec : interface type
//
//A.2.5.1     Property Members
//
//staticopt member ident.opt ident = expr
//
//staticopt member ident.opt ident with get pat = expr
//
//staticopt member ident.opt ident with set patopt pat= expr
//
//staticopt member ident.opt ident with get pat = expr and set patopt pat = expr
//
//staticopt member ident.opt ident with set patopt pat = expr and get pat = expr
//
//A.2.5.2     Method Members
//
//staticopt member ident.opt ident pat1 ... patn = expr
//
//A.2.5.3     Abstract Members
//
//abstract accessopt member-sig
//
// 
//
//member-sig :
//
//    ident typar-defnsopt : curried-sig
//
//    ident typar-defnsopt : curried-sig with get
//
//    ident typar-defnsopt : curried-sig with set
//
//    ident typar-defnsopt : curried-sig with get, set
//
//    ident typar-defnsopt : curried-sig with set, get
//
// 
//
//curried-sig : args-spec1 -> ... -> args-specn -> type
//
//A.2.5.4     Implementation Members
//
//override ident.ident pat1 ... patn = expr
//
//default ident.ident pat1 ... patn = expr
//
//A.2.6      Units Of Measure
//
//measure-literal-atom :
//
//      long-ident
//
//      ( measure-literal-simp )
//
// 
//
//measure-literal-power :
//
//      measure-literal-atom
//
//      measure-literal-atom ^ int32
//
// 
//
//measure-literal-seq :
//
//      measure-literal-power
//
//      measure-literal-power measure-literal-seq
//
// 
//
//measure-literal-simp :
//
//      measure-literal-seq
//
//      measure-literal-simp * measure-literal-simp
//
//      measure-literal-simp / measure-literal-simp
//
//      / measure-literal-simp
//
//      1
//
// 
//
and measure_literal =
|Underscore of underscore
|MLS of measure_literal_simp
// 
//
//const :
//
//      ...
//
//      sbyte < measure-literal >
//
//      int16 < measure-literal >
//
//      int32 < measure-literal >
//
//      int64 < measure-literal >
//
//      ieee32 < measure-literal >
//
//      ieee64 < measure-literal >
//
//      decimal < measure-literal >
//
// 
//
//measure-atom :
//
//      typar
//
//      long-ident
//
//      ( measure-simp )
//
// 
//
//measure-power :
//
//      measure-atom
//
//      measure-atom ^ int32
//
// 
//
//measure-seq :
//
//      measure-power
//
//      measure-power measure-seq
//
// 
//
//measure-simp :
//
//      measure-seq
//
//      measure-simp * measure-simp
//
//      measure-simp / measure-simp
//
//      / measure-simp
//
//      1
//
// 
//
//measure :
//
//      _
//
//     measure-simp
//
//A.2.7      Custom Attributes and Reflection
//
//TODO: def here is unclear
and attribute = Option<attribute_target> *  object_construction
and attribute_set = open_square * lessthan * attribute * List<semicolon * attribute> * greaterthan * close_square 
and attributes = Plus<attribute_set> 
and attribute_target =
|Assembly of assembly_w
|Module of module_w
|Return of return_w
|Field of field_w
|Property of property_w
|Param of param_w
|Type of type_w
|Constructor of constructor_w
|Event of event_w

//A.2.8      Compiler Directives
//
//Compiler directives in non-nested modules or namespace declaration groups:
//
//# id string ... string
//
//A.3 ML Compatibility Features
//
//A.3.1      Conditional Compilation
//
//start-fsharp-token :
//
//      "(*IF-FSHARP"
//
//      "(*F#"
//
//end-fsharp-token :
//
//      "ENDIF-FSHARP*)"
//
//      "F#*)"
//
//start-ml-token : "(*IF-OCAML*)"
//
//end-ml-token : "(*ENDIF-OCAML*)"
//
//A.3.2      Extra Syntactic Forms
//
//ocaml-ident-keyword : one of
//
//      asr land lor lsl lsr lxor mod
//
// 
//
//expr :
//
//      ...
//
//      expr.(expr)           // array lookup
//
//      expr.(expr) <- expr   // array assignment
//
// 
//
//type :
//
//      ...
//
//      (type,...,type) long-ident // generic type instantiation
//
// 
//
//module-implementation :
//
//      ...
//
//      module ident = struct ... end
//
// 
//
//module-signature :
//
//      ...
//
//      module ident : sig ... end
//
//A.3.3      Extra Operators
//
//e1 or e2      → (or) e1 e2
//
//e1 & e2       → (&) e1 e2


type Main =
|Literal of whitespace_or_newline
|BCS of block_comment_start
|BCE of block_comment_end
|ELC of end_of_line_comment
|Hashif of if_directive
|LongID of long_ident
