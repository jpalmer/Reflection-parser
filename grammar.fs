module grammar 

type Literals = 
|i of int
|c of char

type Main =
|Literals of literal list
