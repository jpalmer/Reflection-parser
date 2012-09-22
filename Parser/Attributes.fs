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

type Postfix(c:char) = 
    inherit System.Attribute()
    member x.Postfix = c

type Plus<'t> = 't * ('t list)