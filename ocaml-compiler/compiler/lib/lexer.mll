{
  open Parser
}

let delim = [ ' ' '\t' '\n' ]

let primitive = [ '+' '-' '*' '/' '<' ]

rule program = parse
  | delim+ { program lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "lambda" { LAMBDA }
  | "define" { DEFINE }
  | "cond" { COND }
  | "else" { ELSE }
  | "#t" { BOOL true }
  | "#f" { BOOL false }
  | primitive as c { SYMBOL (Char.escaped c) }
  | ('0' | ['1'-'9']['0'-'9']*) as n { NUM (int_of_string n) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as s { SYMBOL s }
  | eof { EOF }
