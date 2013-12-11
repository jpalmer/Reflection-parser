// Learn more about F# at http://fsharp.net

module JackAsm
open Attributes
type equals = |[<Prefixc('=')>] Dummy
type colon = |[<Prefixc(';')>] Dummy
type space = |[<Prefixc(' ')>] Dummy
type closebrack = |[<Prefixc(')')>] Dummy
type openbrack = |[<Prefixc('(')>] Dummy
type whitespace = |Dummy of Plus<space>
#if WINDOWS
type inside_End_of_line_comment = |[<NotPrefixc([|'\r';'\n';|],true)>] ELcc
type newline = |[<Prefixs("\r\n")>] Nl 
#else
type inside_End_of_line_comment = |[<NotPrefixc([|'\n';|],true)>] ELcc
type newline = |[<Prefixc('\n')>] Nl 
#endif
type end_of_line_comment = |[<Prefixs(@"//")>] Elc of inside_End_of_line_comment list
type intchar = |[<GrabPrefixClass([|System.Globalization.UnicodeCategory.DecimalDigitNumber|])>] D of char
type int_literal = Plus<intchar>

type dest_ = |[<Prefixc('D')>]D |[<Prefixc('M')>] M |[<Prefixc('A')>]A
type dest = dest_ * Option<dest_>
type RHS_dest = |Dest of dest_ |[<Prefixc('1')>]One |[<Prefixc('0')>]Zero
type op = |[<Prefixc('+')>] Plus |[<Prefixc('-')>] Minus |[<Prefixc('&')>] And |[<Prefixc('|')>] Or
type unop = |[<Prefixc('-')>] UMinus |[<Prefixc('!')>] Bang
type jump = |[<Prefixs("JGT")>] JGT |[<Prefixs("JMP")>] JMP |[<Prefixs("JLE")>] JLE |[<Prefixs("JNE")>] JNE  |[<Prefixs("JGE")>] JGE |[<Prefixs("JEQ")>] JEQ
type jumpsource = |De of dest_ |[<Prefixc('0')>]Z

type LabelChar = |[<GrabPrefixClass([|
                                        System.Globalization.UnicodeCategory.LowercaseLetter;
                                        System.Globalization.UnicodeCategory.UppercaseLetter;
                                        System.Globalization.UnicodeCategory.ConnectorPunctuation;
                                        System.Globalization.UnicodeCategory.DecimalDigitNumber;
                                        System.Globalization.UnicodeCategory.OtherPunctuation;
                                        System.Globalization.UnicodeCategory.CurrencySymbol |])>] LC of char
type label = Plus<LabelChar>
type cinstruc = 
    |Op of dest * equals * dest_ * op * RHS_dest 
    |Unop of dest * equals * unop * RHS_dest 
    |Assign of dest * equals * RHS_dest  
    |Value of jumpsource * option<colon * jump> //no point just having A - need to jump so should always be some
type ainstruc = 
    |[<Prefixc('@')>] Literal of int_literal
    |[<Prefixc('@')>] ALabel of label
    
type line = 
    |Ainstruc of ainstruc
    |Cinstruc of cinstruc * option<colon * jump>
    |LabelDef of openbrack * label * closebrack
type main = |L of (Option<Option<whitespace> * line> * Option<whitespace> * Option<end_of_line_comment> * newline) list
