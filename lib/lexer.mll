{
[@@@coverage exclude_file]

open Parser

exception SyntaxError of string

let parse_int (module S : Checked_oint.S) s =
    let s = Str.(global_replace (regexp "_") "" s) in
    try INT S.(to_generic (of_string_exn s)) with
    | Checked_oint.Out_of_range ->
      raise (SyntaxError (Printf.sprintf "An illegal integer literal: `%s`" s))
;;
}

let blank = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let alnum = (letter | digit)

let symbol = (letter | '_') alnum* '?'? '\''*
let symbol = symbol |
    "~" | "#" |
    "+" | "-" | "*" | "/" | "%" |
    "|" | "&" | "^" | "<<" | ">>" |
    "=" | "!=" |
    ">" | ">=" | "<" | "<=" |
    "++"

let unsigned_0b = ("0b" | "0B") ['0'-'1' '_']+
let unsigned_0o = ("0o" | "0O") ['0'-'7' '_']+
let unsigned_0x = ("0x" | "0X") ['0'-'9' 'a'-'f' 'A'-'F' '_']+
let unsigned_decimal = digit (digit | "_")*
let unsigned = (unsigned_0b | unsigned_0o | unsigned_0x | unsigned_decimal)
let signed = '-'? unsigned

(* The order of tokens should be the same as in [parser.mly]. *)
rule read =
    parse
    | blank { read lexbuf }
    | newline { Lexing.new_line lexbuf; read lexbuf }

    (* Punctuation symbols. *)
    | "," { COMMA }
    | ";" { SEMICOLON }
    | ":=" { DEFINE }
    | "->" { RARROW }

    (* Brackets. *)
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }

    (* Keywords. *)
    | "match" { MATCH }
    | "let" { LET }
    | "call" { CALL }
    | "@extract" { EXTRACT }

    (* Integers. *)
    | (unsigned as s) "u8" { parse_int (module Checked_oint.U8) s }
    | (unsigned as s) "u16" { parse_int (module Checked_oint.U16) s }
    | (unsigned as s) "u32" { parse_int (module Checked_oint.U32) s }
    | (unsigned as s) "u64" { parse_int (module Checked_oint.U64) s }
    | (unsigned as s) "u128" { parse_int (module Checked_oint.U128) s }
    | (signed as s) "i8" { parse_int (module Checked_oint.I8) s }
    | (signed as s) "i16" { parse_int (module Checked_oint.I16) s }
    | (signed as s) "i32" { parse_int (module Checked_oint.I32) s }
    | (signed as s) "i64" { parse_int (module Checked_oint.I64) s }
    | (signed as s) "i128" { parse_int (module Checked_oint.I128) s }

    (* Strings. *)
    | '"' { read_string (Buffer.create 64) lexbuf }

    (* Characters. *)
    | '\'' {
        let c = read_char lexbuf in
        read_quote lexbuf;
        c
    }

    (* Comments. *)
    | "//" { read_comment lexbuf }

    (* Symbols. *)
    | symbol { SYMBOL (Symbol.of_string (Lexing.lexeme lexbuf)) }

    | _ {
        raise (SyntaxError (
            Printf.sprintf "An unexpected character: `%s`" (Lexing.lexeme lexbuf)))
    } 
    | eof { EOF }

and read_string buf =
    parse
    | '"' { STRING (Buffer.contents buf) }
    | '\\' '"' {
        Buffer.add_char buf '"';
        read_string buf lexbuf
    }
    | '\\' {
        Buffer.add_char buf (read_escape_char lexbuf);
        read_string buf lexbuf
    }
    | '\n' {
        Lexing.new_line lexbuf;
        Buffer.add_char buf '\n';
        read_string buf lexbuf
    }
    | [^ '"' '\\' '\n']+ {
        Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
    }
    | _ {
        raise (SyntaxError (
            Printf.sprintf "An unexpected string character: `%s`" (Lexing.lexeme lexbuf)))
    }
    | eof { raise (SyntaxError ("The string is not terminated")) }

and read_char =
    parse
    | '\\' '\'' { CHAR '\'' }
    | '\\' { CHAR (read_escape_char lexbuf) }
    | [^ '\''] { CHAR (Lexing.lexeme_char lexbuf 0) }

and read_quote =
    parse
    | '\'' { () }
    | _ { raise (SyntaxError ("The character is not terminated")) }

and read_escape_char =
    parse
    | 'f' { '\012' }
    | 'n' { '\n' }
    | 'r' { '\r' }
    | 't' { '\t' }
    | 'v' { '\011' }
    | 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] {
        char_of_int (int_of_string ("0" ^ Lexing.lexeme lexbuf))
    }
    | '\\' { '\\' }

and read_comment =
    parse
    | [^ '\n']* { COMMENT (Lexing.lexeme lexbuf) }
    | eof { EOF }
