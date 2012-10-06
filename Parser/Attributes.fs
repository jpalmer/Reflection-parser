module Attributes

type Prefixc(c:char) =
    inherit System.Attribute()
    member x.Prefix = c

type Prefixs(s:string) =
    inherit System.Attribute()
    member x.Prefix = s

type NotPrefixc(c:char[]) = 
    inherit System.Attribute()
    member x.Prefix = c

type GrabPrefixClass(c:System.Globalization.UnicodeCategory[]) = 
    inherit System.Attribute()
    member x.Prefix = c

//parser codes assumes this is not used with GrabPrefixClass
type Anychar() = 
    inherit System.Attribute()


type Plus<'t> = 't * ('t list)