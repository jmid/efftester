Effect-Driven Compiler Tester:
==============================

This is a prototype implementation of a compiler testing approach
described in the paper:

    Effect-Driven QuickChecking of Compilers
    Jan Midtgaard, Patrick Kasting, Mathias Nygaard Justesen, Flemming Nielson, Hanne Riis Nielson
    ICFP'17

We suggest to generate programs following a dedicated type and effect
system that determines whether a program has evaluation-order
dependence. By reading the type-and-effect-system relation bottom up,
in a goal-directed manner we can generate programs that are free of
evaluation-oder dependence. We can thus run such programs with both
the bytecode and native code backends of OCaml, log their observable
behaviour to a file, and `diff` the two behaviours.


Building and running:
---------------------

The tester requires OCaml, the QCheck library, and a *nix-like platform with `diff` and `/dev/null`.

To build simply run `make`.

Now run `effmain.native -v` (the native code build is recommended for speed reasons).
This should produce output along these lines:

```
$ ./effmain.native -v
random seed: 228021772
...........................................................................x...x...x...x...x...x...x...x...x...x....x...x..x....x....x.....x..x...x..x......x.....x.....x.....x.....x.....x.....x.....x.....x.....x.....x.....x.....x.....x.....
law bytecode/native backends agree: 76 relevant cases (76 total)

  test `bytecode/native backends agree`
  failed on ≥ 1 cases:
  Some (let o = (let j = List.hd [] in fun w -> int_of_string) "" in 0) (after 32 shrink steps)
  
failure (1 tests failed, ran 1 tests)
```

Interestingly, the produced counterexamples such as
`let o = (let j = List.hd [] in fun w -> int_of_string) "" in 0`
above illustrate observable differences in the executables produced by
the two backend, aka, bugs. This particular one is a variant of MPR#7531
listed below.

You can provide a known seed with the flag `-s`: `./effmain.native -v -s 228021772`
to reproduce an earlier run.

When invoked without a command-line seed QCheck is seeded differently
for each run and will therefore (most likely) produce different
results for each of them:
- each run may reveal different errors or
- no bugs are found in the given run.

The reproducability with respect to a given seed should be taken with
a large grain of salt. A seed may still yield different results
depending on the underlying architecture, OS, bit-width,
OCaml-version, and QCheck-version. Furthermore it is (all too) easy to
invalidate previous seeds by any adjustments to
- the generator -- as the adjusted version may utilize/interpret the
  old seed differently,
- the generator's initial environment -- as choices herein may come
  out differently for a smaller/bigger/different environment,
- the shrinker -- as the adjusted version may shrink a bigger
  problematic counterexample to a different minimal example.

The generated programs are written to `testdir/test.ml` and the
compiled versions are named `testdir/native` and `testdir/byte`.
These are overwritten on each iteration. As such, the last version
remaining rarely represents a counterexample. Instead it represents
the shrinker's last (failed) attempt to shrink the minimal
counterexample one step further.

The current version tests OCaml version 4.04.0's native-code compiler
with flag `-O3` against the bytecode compiler. It was originally
tested with version 4.02.3 which doesn't support the `-O3` flag. So
far the tester has found the same bugs (below) related to erroneous
optimization in 4.02.3 as in 4.04.0 with the `-O3` flag.

If you find more errors using this approach please let me know.


Bugs found:
-----------

MPR#7531  https://caml.inria.fr/mantis/view.php?id=7531  Delayed effects in partially applied functions

MPR#7533  https://caml.inria.fr/mantis/view.php?id=7533  Wrong code generation of `e / e'` and `e mod e'` with effectful `e`

MPR#7201  https://caml.inria.fr/mantis/view.php?id=7201  Wrong optimization of `0 / e`
	  						 (reported by way of Jean-Christophe Filliatre)


Known bugs recreated:
---------------------

GPR#954  https://github.com/ocaml/ocaml/pull/954  Wrong optimisation of `0 mod e`
                                                  (fixes both the division and mod cases)

GPR#956  https://github.com/ocaml/ocaml/pull/956  Keep possibly-effectful expressions when optimizing multiplication by zero



Observing the generated programs:
---------------------------------

To invoke the generator directly, first `make eff` and then start
`ocaml` from within this directory (this will load modules suitably
via `.ocamlinit`).

To generate an arbitrary program you can now invoke `Gen.generate1
term_gen.gen` (wrapped with a suitable string coersion to make the
generator's output more understandable).  The output changes per each
invocation:

```
$ ocaml
        OCaml version 4.04.0

[some lines about loaded libs omitted]

# Print.option (toOCaml ~typeannot:false) (Gen.generate1 term_gen.gen);;
- : string = "Some (exit ((fun n -> (-98051756132636271)) string_of_bool))"

# Print.option (toOCaml ~typeannot:false) (Gen.generate1 term_gen.gen);;
- : string =
"Some ((mod) (pred (let h = if true then exit ((/) (if false then 1830787755246062127
 else (-2895157864674163253)) (lnot ((asr) (-3089269914618536456) 93))) else 
 let w = print_endline (let i = lnot 996022529208063915 in string_of_int 25) in 
 string_of_bool (exit (abs (-427726557501168681))) in ( * ) (pred (lnot (List.hd 
 (List.hd (exit 991))))) (lnot 5))) ((lsr) 225 755))"
```


To observe the distribution of the effect-driven program generator you
can build a different target with `make stat` which results in a
`effstat.native` executable. This program runs a constant true test
over generated terms while logging the size of each term.

A bashscript `runstat.sh` will then run `./effstat.native -v`, log the
output, process it with `sed`, and pass the output to the program
'ministat'.

`runstat.sh` itself requires a *nix platform with bash and 'ministat'
installed.

When run, `runstat.sh` should produce output along these lines (you
can adjust the 50 generated terms in `src/effstat.ml`):
```
$ ./runstat.sh 
x stat_size.txt
+-------------------------------------------------------------------------------------------------------------+
|             x                                                                                               |
|            xx                                                                                               |
|            xx                                                                                               |
|           xxx                                                                                               |
|          xxxx                                                                                               |
|          xxxx                                                                                               |
|          xxxx                                                                                               |
|          xxxx                                                                                               |
|          xxxx    x x              xx                                                                        |
|          xxxxx   xxx    x x      xxx                 x                            x x     x                x|
||____________M_________A_____________________|                                                               |
+-------------------------------------------------------------------------------------------------------------+
    N           Min           Max        Median           Avg        Stddev
x  50             1           639            16         80.82     148.41691
```

The very latest version of the QCheck library can provide similar
output and thereby removes the need for the above bashscript.
