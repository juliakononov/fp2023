(** Copyright 2021-2023, Lesh79 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

type binOp =
  | Plus (* Бинарная операция сложнния '+' *)
  | Minus (* Бинарная операция вычитания '-' *)
  | Multi (* Бинарная операция умножения '*' *)
  | Division (* Бинарная операция деления '/' *)
  | And (* Бинарная логическая '&&' *)
  | Or (* Бинарная логическая '||' *)
  | Equal (* Бинарная операция равентсва '==' *)
  | NotEqual (* Бинарная операция неравенства '<>' *)
  | Lt (* Бинарная операция меньше '<' *)
  | Ltq (* Бинарная операция меньше или равно '<=' *)
  | Gt (* Бинарная операция больше '>' *)
  | Gtq (* Бинарная операция больше или равно '>=' *)
  | Cons (* Начало списка '::' *)
[@@deriving eq, show { with_path = false }]

type expr =
  | EInt of int
  | EString of string
  | EBool of bool
  | EVar of id (* Выражения имен переменных *)
  | EBinop of binOp * expr * expr (* Выражения бинарных операций *)
  | EIfThenElse of expr * expr * expr (* Условный оператор If then else *)
  | Eapp of expr * expr
  | ELet of bool * id * expr * expr (* Представляет конструкцию Let*)
  | EFun of id * expr
[@@deriving eq, show { with_path = false }]

let e_int int = EInt int
let e_string string = EString string
let e_bool bool = EBool bool
let e_var str = EVar str
let e_binop op exp1 exp2 = EBinop (op, exp1, exp2)
let e_ifthenelse exp1 expr2 expr3 = EIfThenElse (exp1, expr2, expr3)
let e_app expr1 expr2 = Eapp (expr1, expr2)
let e_let bool id exp1 exp2 = ELet (bool, id, exp1, exp2)
let e_fun id exp = EFun (id, exp)

type decl = DeclLet of bool * id * expr [@@deriving eq, show { with_path = false }]

let d_decl bool id exp = DeclLet (bool, id, exp)

type program = decl list [@@deriving eq, show { with_path = false }]
