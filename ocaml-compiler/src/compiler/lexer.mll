{
  open Parser
}

let delim = [ ' ' '\t' '\n' ]

rule program = parse
  | delim+ { program lexbuf }
  | eof { EOF }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '.' { DOT }
  | '`' { QUOTE }
  | "lambda" { LAMBDA }
  | "define" { DEFINE }
  | "if" { IF }
  | "cond" { COND }
  | "else" { ELSE }
  | "let" { LET }
  | "begin" { BEGIN }
  | '0' | ['1'-'9']['0'-'9']* as n { NUM n }
  | ['a'-'z' 'A'-'Z' '0'-'9' '#' '?' '+' '-' '*' '/' '<' '>' '=' '_' ]+ as s { ID s }
