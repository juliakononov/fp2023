(** Copyright 2021-2022, *)

(** SPDX-License-Identifier: CC0-1.0 *)
open DAhmetov_lib.Ast

open DAhmetov_lib.Parser

let () =
  let test = "let rec fact = fun n -> if n < 2 then 1 else n * fact n-1" in
  match parse test with
  | Result.Ok res -> Format.printf "%a\n" pp_program res
  | _ -> Format.printf "Nado trenirovatsa"
;;
