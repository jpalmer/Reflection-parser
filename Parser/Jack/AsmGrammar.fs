// Learn more about F# at http://fsharp.net

module JackAsm
open Attributes
type equals = |[<Prefixc('=')>] Dummy
type colon = |[<Prefixc(';')>] Dummy
type zero = |[<Prefixc('0')>] Dummy
type one = |[<Prefixc('1')>] Dummy
type space = |[<Prefixc(' ')>] Dummy
type closebrack = |[<Prefixc(')')>] Dummy
type openbrack = |[<Prefixc('(')>] Dummy
type whitespace = |Dummy of Plus<space>
#if WINDOWS
type inside_End_of_line_comment = |[<NotPrefixc([|'\r';'\n';|],true)>] ELcc
type newline = |[<Prefixs("\r\n")>] Nl //should add support for linux newlines
#endif
type end_of_line_comment = |[<Prefixs(@"//")>] Elc of inside_End_of_line_comment list
type intchar = |[<GrabPrefixClass([|System.Globalization.UnicodeCategory.DecimalDigitNumber|])>] D of char
type int_literal = |Li of intchar list

type dest_ = |[<Prefixc('D')>]D |[<Prefixc('M')>] M |[<Prefixc('A')>]A
type dest = dest_ * Option<dest_>
type RHS_dest = |Dest of dest_ |One of one |Zero of zero
type op = |[<Prefixc('+')>] Plus |[<Prefixc('-')>] Minus |[<Prefixc('&')>] And |[<Prefixc('|')>] Or
type unop = |[<Prefixc('-')>] UMinus |[<Prefixc('!')>] Bang
type jump = |[<Prefixs("JGT")>] JGT |[<Prefixs("JMP")>] JMP |[<Prefixs("JLE")>] JLE |[<Prefixs("JNE")>] JNE  |[<Prefixs("JGE")>] JGE
type jumpsource = |De of dest_ |Z of zero

type LabelChar = |[<GrabPrefixClass([|
                                        System.Globalization.UnicodeCategory.LowercaseLetter;
                                        System.Globalization.UnicodeCategory.UppercaseLetter;
                                        System.Globalization.UnicodeCategory.ConnectorPunctuation |])>] LC of char
type label = |Label of Plus<LabelChar>
type cinstruc = 
    |Op of dest * equals * dest_ * op * RHS_dest * option<colon * jump>
    |Unop of dest * equals * unop * RHS_dest * option<colon * jump>
    |Assign of dest * equals * RHS_dest  * option<colon * jump>
    |Value of jumpsource * option<colon * jump> //no point just having A - need to jump so should always be some
type ainstruc = 
    |[<Prefixc('@')>] ALabel of label
    |[<Prefixc('@')>] Literal of int_literal
    
type line = 
    |Ainstruc of ainstruc
    |Cinstruc of cinstruc
    |LabelDef of openbrack * label * closebrack
type main = |L of (Option<Option<whitespace> * line> * Option<whitespace> * Option<end_of_line_comment> * newline) list
