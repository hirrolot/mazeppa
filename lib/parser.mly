// TODO: implement proper error messages. See "11. Error handling: the new way"
// from the Menhir reference manual.

%{
[@@@coverage exclude_file]
%}

// The order of tokens should be the same as in [lexer.mll].

// Punctuation symbols.
%token COMMA SEMICOLON DEFINE RARROW

// Brackets.
%token LPAREN RPAREN LBRACE RBRACE

// Keywords.
%token MATCH LET EXTRACT CALL

// Miscellaneous.
%token <Checked_oint.generic> INT
%token <string> STRING
%token <char> CHAR
%token <string> COMMENT
%token <Symbol.t> SYMBOL

%token EOF

%start <Raw_program.t> program

%%

program:
    | defs = file_def_list { defs }

file_def_list:
    | attrs = def_attr_list; f = SYMBOL; LPAREN;
        params = separated_list(COMMA, SYMBOL); RPAREN; DEFINE; body = term; SEMICOLON;
        rest = file_def_list {
        (attrs, f, params, body) :: rest
    }

    | COMMENT; rest = file_def_list { rest }

    | EOF { [] }

def_attr_list:
    | EXTRACT; rest = def_attr_list { `Extract :: rest }
    | { [] }

term:
    | x = SYMBOL { Raw_term.Var x }

    | const = const { Raw_term.Const const }

    | op = SYMBOL; LPAREN; args = separated_list(COMMA, term); RPAREN {
        Raw_term.Call (op, args)
    }

    | CALL; op = STRING; LPAREN; args = separated_list(COMMA, term); RPAREN {
        let ffi_pattern = Str.regexp "^mz_ffi_[a-zA-Z_][a-zA-Z0-9_]*$" in
        if not (Str.string_match ffi_pattern op 0) then
            Util.panic "FFI functions must be named `mz_ffi_xxx`: `%s`" op;
        Raw_term.Call (Symbol.of_string op, args)
    }

    | MATCH; t = term;
        LBRACE; cases = separated_nonempty_list(COMMA, match_case); RBRACE {
        Raw_term.Match (t, cases)
    }

    | LET; x = SYMBOL; DEFINE; t = term; SEMICOLON; u = term {
        Raw_term.Let (x, t, u)
    }

    | LET; p = pattern; DEFINE; t = term; SEMICOLON; u = term {
        Raw_term.Match (t, [ p, u ])
    }

    | COMMENT; t = term { t }

const:
    | x = INT { Const.Int x }
    | s = STRING { Const.String s }
    | c = CHAR {
        let open Checked_oint in
        let x = U8.of_int_exn (int_of_char c) in
        Const.Int (U8 x)
    }

match_case:
    | p = pattern; RARROW; t = term {
        p, t
    }

pattern:
    | c = SYMBOL; LPAREN; c_params = separated_list(COMMA, SYMBOL); RPAREN {
        c, c_params
    }
