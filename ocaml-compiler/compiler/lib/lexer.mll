{
  open Parser
}

let delim = [ ' ' '\t' '\n' ]

rule program = parse
  | delim+ { program lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "lambda" { LAMBDA }
  | "define" { DEFINE }
  | "#t" { BOOL(true) }
  | "#f" { BOOL(false) }
  | ('0' | ['1'-'9']['0'-'9']*) as n { NUM(int_of_string n) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as s { SYMBOL(s) }
  | eof { EOF }
