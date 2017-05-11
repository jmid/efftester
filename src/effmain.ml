open Efftester;;

resetvar();;
resettypevar();;
QCheck_runner.run_tests_main
  [(* unify_funtest;*)
   (* gen_classify; *)
   (* ocaml_test; *)
   (* tcheck_test; *)
    eq_test 
  ]
