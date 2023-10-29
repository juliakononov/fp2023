(** Copyright 2021-2023, Lesh79 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Format

type input = char list

let is_empty = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_Uletter = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "fun"
  | "true"
  | "false"
  | "in"
  | "bool"
  | "string"
  | "int"
  | "match"
  | "with"
  | "if"
  | "then"
  | "else"
  | "for"
  | "while" -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let is_iddent_symbol = function
  | c -> is_digit c || is_letter c || is_Uletter c || Char.equal c '_'
;;

let empty = take_while is_empty
let empty1 = take_while1 is_empty
let trim p = empty *> p
let trim1 p = empty1 *> p
let token s = empty *> string s
let between p = token "(" *> p <* token ")"

let pIdent =
  trim
  @@ lift2
       (fun hd tl -> Char.escaped hd ^ tl)
       (satisfy (fun ch -> ch = '_' || is_letter ch))
       (take_while (fun ch -> ch = '_' || is_letter ch || is_Uletter ch || is_digit ch))
  >>= fun s ->
  if is_keyword s
  then fail "keyword reserved"
  else if s = "_"
  then fail "wildcard `_` not supported"
  else return s
;;

(*-------------------------------------------------------------------*)
(* types *)
let pvar = trim @@ take_while is_iddent_symbol
let pstring = trim @@ (token "\"" *> take_while is_iddent_symbol) <* token "\""
let pint = int_of_string <$> trim @@ take_while1 is_digit
let pbool = choice [ token "true" *> return true; token "false" *> return false ]

(* Operation *)
let peMul pe =
  chainl1 pe (trim @@ (token "*" *> return (fun ex1 ex2 -> e_binop Multi ex1 ex2)))
;;

let peDiv pe =
  chainl1 pe (trim @@ (token "/" *> return (fun ex1 ex2 -> e_binop Division ex1 ex2)))
;;

let pePlus pe =
  chainl1 pe (trim @@ (token "+" *> return (fun ex1 ex2 -> e_binop Plus ex1 ex2)))
;;

let peMinus pe =
  chainl1 pe (trim @@ (token "-" *> return (fun ex1 ex2 -> e_binop Minus ex1 ex2)))
;;

let peEq pe =
  chainl1 pe (trim @@ (token "==" *> return (fun ex1 ex2 -> e_binop Equal ex1 ex2)))
;;

let peNotEq pe =
  chainl1 pe (trim @@ (token "<>" *> return (fun ex1 ex2 -> e_binop NotEqual ex1 ex2)))
;;

let peLt pe =
  chainl1 pe (trim @@ (token "<" *> return (fun ex1 ex2 -> e_binop Lt ex1 ex2)))
;;

let peLtq pe =
  chainl1 pe (trim @@ (token "<=" *> return (fun ex1 ex2 -> e_binop Ltq ex1 ex2)))
;;

let peGt pe =
  chainl1 pe (trim @@ (token ">" *> return (fun ex1 ex2 -> e_binop Gt ex1 ex2)))
;;

let peGtq pe =
  chainl1 pe (trim @@ (token ">=" *> return (fun ex1 ex2 -> e_binop Gtq ex1 ex2)))
;;

(* Same operation*)

let peApp pe = chainl1 pe (return (fun exp1 exp2 -> e_app exp1 exp2))

let pif pe =
  trim @@ lift3 e_ifthenelse (token "if" *> pe) (token "then" *> pe) (token "else" *> pe)
;;

let pefun pe = trim @@ (token "fun" *> lift2 e_fun pIdent (trim @@ (token "->" *> pe)))

let pelet pe =
  token "let"
  *> lift4
       e_let
       (token "rec" *> return true <|> return false)
       pIdent
       (token "=" *> pe)
       (token "in" *> pe)
;;

let pexp =
  fix
  @@ fun pexp ->
  let pe = between pexp in
  let pe = e_var <$> pIdent <|> pe in
  let pe = e_string <$> pstring <|> pe in
  let pe = e_bool <$> pbool <|> pe in
  let pe = e_int <$> pint <|> pe in
  let pe = peApp pe <|> pe in
  let pe = pePlus pe <|> pe in
  let pe = peMinus pe <|> pe in
  let pe = peMul pe <|> pe in
  let pe = peDiv pe <|> pe in
  let pe = peEq pe <|> pe in
  let pe = peNotEq pe <|> pe in
  let pe = peLt pe <|> pe in
  let pe = peLtq pe <|> pe in
  let pe = peGt pe <|> pe in
  let pe = peGt pe <|> pe in
  let pe = peGtq pe <|> pe in
  let pe = pif pe <|> pe in
  let pe = pefun pe <|> pe in
  let pe = pelet pe <|> pe in
  pe
;;

let pdecl =
  token "let"
  *> lift3 d_decl (token "rec" *> return true <|> return false) pIdent (token "=" *> pexp)
;;

let parse_program = many pdecl <* empty
let parse str = parse_string ~consume:All parse_program str

let test_parse str expected =
  match parse str with
  | Ok actual ->
    let is_eq = List.equal equal_decl expected actual in
    if is_eq then () else printf "Actual %a\n" pp_program actual;
    is_eq
  | Error err ->
    printf "%s\n" err;
    false
;;

let%test _ = test_parse "let f = 5" [ DeclLet (false, "f", EInt 5) ]

let%test _ =
  test_parse
    "let rec fact = fun n -> if n < 2 then 1 else n * fact n-1"
    [ DeclLet
        ( true
        , "fact"
        , EFun
            ( "n"
            , EIfThenElse
                ( EBinop (Lt, EVar "n", EInt 2)
                , EInt 1
                , EBinop
                    (Multi, EVar "n", EBinop (Minus, Eapp (EVar "fact", EVar "n"), EInt 1))
                ) ) )
    ]
;;

let%test _ =
  test_parse
    "let vshi = a + 5"
    [ DeclLet (false, "vshi", EBinop (Plus, EVar "a", EInt 5)) ]
;;

let%test _ =
  test_parse
    "let vshi = if 5 then f5 else dfg"
    [ DeclLet (false, "vshi", EIfThenElse (EInt 5, EVar "f5", EVar "dfg")) ]
;;

let%test _ =
  test_parse
    "let vshi = if 5 + 5 then 10 else 0"
    [ DeclLet (false, "vshi", EIfThenElse (EBinop (Plus, EInt 5, EInt 5), EInt 10, EInt 0))
    ]
;;

let%test _ =
  test_parse
    "let binops  = 5 + 5 * 10 + 10 / 2"
    [ DeclLet
        ( false
        , "binops"
        , EBinop
            ( Division
            , EBinop
                (Multi, EBinop (Plus, EInt 5, EInt 5), EBinop (Plus, EInt 10, EInt 10))
            , EInt 2 ) )
    ]
;;

(*========================================================================================*)
