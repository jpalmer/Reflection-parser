module Attributes

type Prefix(c:char) =
    inherit System.Attribute()
    member x.Prefix = c

type Postfix(c:char) = 
    inherit System.Attribute()
    member x.Postfix = c