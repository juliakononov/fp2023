  $ dune exec demo_fact
  [(DeclLet (true, "fact",
      (EFun ("n",
         (EIfThenElse ((EBinop (Lt, (EVar "n"), (EInt 2))), (EInt 1),
            (EBinop (Multi, (EVar "n"),
               (EBinop (Minus, (Eapp ((EVar "fact"), (EVar "n"))), (EInt 1)))))
            ))
         ))
      ))
    ]
