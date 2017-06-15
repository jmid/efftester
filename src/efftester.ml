(** *************************************************************** *)
(** OCaml compiler backend tester                                   *)
(** Initial version by Patrick Kasting and Mathias Nygaard Justesen *)
(** Type and effect extension by Jan Midtgaard                      *)
(** *************************************************************** *)

open QCheck

(** Compilation *)

(* Write OCaml source to file *)
let write_prog src filename =
  let ostr = open_out filename in
  let () = output_string ostr src in
  close_out ostr

let run srcfile compname compcomm =
  let exefile = "testdir/" ^ compname in
  let exitcode =
    Sys.command (compcomm ^ " " ^ srcfile ^ " -o " ^ exefile) in
  (* Check that compilation was successful *)
  if exitcode <> 0
  then
    failwith (compname ^ " compilation failed with error code " ^ (string_of_int exitcode))
  else
    (* Run the compiled program *)
    let runcode = Sys.command ("./" ^ exefile ^ " >" ^ exefile ^ ".out 2>&1") in
    (runcode, exefile ^ ".out")
  
let nativeByteEquivalence (*printFunction*) src =
  (* Write OCaml source to file *)
  let file = "testdir/test.ml" in
  let () = write_prog src file in   (* -w -5@20-26 *)
  let nrun = run file "native" "ocamlopt -O3 -w -5-26" in (* Silence warnings for partial applications *)
  let brun = run file "byte" "ocamlc -w -5-26" in         (*                      and unused variables *)
  match nrun,brun with
    | (ncode,nout), (bcode,bout) ->
      let comp = Sys.command ("diff -q " ^ nout ^ " " ^ bout ^ " > /dev/null") in
      let res = ncode = bcode && comp = 0 in
	if res
	then (print_string "."; flush stdout; res)
	else (print_string "x"; flush stdout; res)


(** AST type definitions  *)

type variable = string

type eff = bool * bool

type typevar = int

let resettypevar,newtypevar =
  let count = ref 0 in
  (fun () -> count := 0),
  (fun () -> let old = !count in
	     incr count;
	     old)

let resetvar,newvar =
  let count = ref 0 in
  (fun () -> count := 0),
  (fun () -> let old = !count in
	     incr count;
	     "var" ^ (string_of_int old))

let reseteffvar,neweffvar =
  let count = ref 0 in
  (fun () -> count := 0),
  (fun () -> let old = !count in
	     incr count;
	     "eff" ^ (string_of_int old))
    
type etype =
  | Typevar of typevar
  | Unit
  | Int
  | Bool
  | String
  | List of etype
  | Fun of etype * eff * etype

let rec ftv = function
  | Typevar a -> [a]
  | Unit
  | Int
  | Bool
  | String    -> []
  | List et   -> ftv et
  | Fun (a,_,r) -> (ftv a)@(ftv r)

type lit =
  | LitUnit
  | LitInt of int
  | LitBool of bool
  | LitStr of string
  | LitList of lit list

type term =
  | Lit of lit
  | Variable of etype * variable
  | Lambda of etype * variable * etype * term
  | App of etype * term * etype * term * eff
  | Let of variable * etype * term * term * etype * eff
  | If of etype * term * term * term * eff


(** Printing functions  *)
      
let rec typeToOCaml ?(effannot=false) sb = function
  | Typevar a  -> Buffer.add_string sb ("'a" ^ (string_of_int a))
  | Unit       -> Buffer.add_string sb "unit"
  | Int        -> Buffer.add_string sb "int"
  | Bool       -> Buffer.add_string sb "bool"
  | String     -> Buffer.add_string sb "string"
  | List s     ->
    (Buffer.add_string sb "(";
     typeToOCaml ~effannot:effannot sb s;
     Buffer.add_string sb ") list")
  | Fun (s, e, t) ->
    (match s with
      | Unit 
      | Int
      | Bool
      | String ->
        (typeToOCaml ~effannot:effannot sb s;
	 if effannot
	 then
	   (Buffer.add_string sb " -";
	    Buffer.add_string sb (string_of_bool (fst e));
            Buffer.add_string sb "/";
	    Buffer.add_string sb (string_of_bool (snd e));
            Buffer.add_string sb "-> ")
	 else
	   Buffer.add_string sb " -> ";
         typeToOCaml ~effannot:effannot sb t)
      | _      ->
        (Buffer.add_string sb "(";
         typeToOCaml ~effannot:effannot sb s;
         Buffer.add_string sb ")";
	 if effannot
	 then
	   (Buffer.add_string sb " -";
	    Buffer.add_string sb (string_of_bool (fst e));
            Buffer.add_string sb "/";
	    Buffer.add_string sb (string_of_bool (snd e));
            Buffer.add_string sb "-> ")
	 else
	   Buffer.add_string sb " -> ";
         typeToOCaml ~effannot:effannot sb t))

let typetostr ?(effannot=false) typ =
  let sb = Buffer.create 20 in
  let () = typeToOCaml ~effannot:effannot sb typ in
  Buffer.contents sb

let efftostr ((ef,ev):eff) =
  "(" ^ (string_of_bool ef) ^ "," ^ (string_of_bool ev) ^ ")"


(* BNF grammar:

    exp ::= l | x | fun (x:t) -> exp | exp exp | let (x:t) = exp in exp
 
   Same language but unambiguously formulated grammar:

    exp ::= app | fun (x:t) -> exp | let (x:t) = exp in exp | if exp then exp else exp
    app ::= arg | app arg
    arg ::= l | x | (exp)

   The following prettyprinter is structured according to this grammar to cut down on
   the needless parentheses
 *)
let toOCaml ?(typeannot=true) term =
  let rec littoOcamlSB sb = function
    | LitUnit    -> Buffer.add_string sb "()"
    | LitInt i   ->
      if i < 0
      then
	(Buffer.add_string sb "(";
	 Buffer.add_string sb (string_of_int i);
	 Buffer.add_string sb ")")
      else
	 Buffer.add_string sb (string_of_int i)
    | LitBool b  -> Buffer.add_string sb (string_of_bool b)
    | LitStr s   ->
      (Buffer.add_string sb "\"";
       Buffer.add_string sb s;
       Buffer.add_string sb "\"")
    | LitList ls  ->
      (Buffer.add_string sb "[";
       List.iter (fun l -> (littoOcamlSB sb l;
			    Buffer.add_string sb "; ")) ls;
       Buffer.add_string sb "]")
  in
  let rec exp sb t = match t with
    | Lambda (_, x, t, m) ->
      (Buffer.add_string sb "fun ";
       if typeannot
       then
	 (Buffer.add_string sb "(";
	  Buffer.add_string sb x;
	  Buffer.add_string sb ":";
	  typeToOCaml sb t; 
	  Buffer.add_string sb ")")
       else 
	  Buffer.add_string sb x;
       Buffer.add_string sb " -> ";
       exp sb m)
    | Let (x, t, m, n, _, _) ->
      (Buffer.add_string sb "let ";
       if typeannot
       then
	 (Buffer.add_string sb "(";
	  Buffer.add_string sb x;
	  Buffer.add_string sb ":";
	  typeToOCaml sb t; 
	  Buffer.add_string sb ")")
       else
	  Buffer.add_string sb x;
       Buffer.add_string sb " = ";
       exp sb m;
       Buffer.add_string sb " in ";
       exp sb n)
    | If (_,b,m,n,_) ->
      (Buffer.add_string sb "if ";
       exp sb b;
       Buffer.add_string sb " then ";
       exp sb m;
       Buffer.add_string sb " else ";
       exp sb n)
    | _ -> app sb t
  and app sb t = match t with
    | App (_, m, _, n, e) ->
      (app sb m;
       Buffer.add_string sb " ";
       arg sb n)
    | _ -> arg sb t
  and arg sb t = match t with
    | Lit l           -> littoOcamlSB sb l
    | Variable (_, s) -> Buffer.add_string sb s
    | _               -> 
      (Buffer.add_string sb "(";
       exp sb t;
       Buffer.add_string sb ")")
  in
  let sb = Buffer.create 80 in
  let () = exp sb term in
  Buffer.contents sb


(** Effect system function *)

let no_eff = (false,false)
    
let eff_join (ef,ev) (ef',ev') = (ef || ef', ev || ev')

let eff_leq eff eff_exp = match eff,eff_exp with
  | (false,true), _ | _, (false,true) -> failwith "eff_leq: this should not happen"
  | (false,false), _         -> true (* no eff, compat with anything *)
  | (true,false), (true,_)   -> true
  | (true,true), (true,true) -> true
  | _,_ -> false

let rec occurs tvar = function
  | Typevar a   -> tvar = a
  | List a      -> occurs tvar a
  | Fun (a,_,b) -> occurs tvar a || occurs tvar b
  | _           -> false

let rec arity = function
  | Fun (_,_,b) -> 1 + arity b
  |  _          -> 0

let rec subst repl t = match t with
  | Unit
  | Int
  | Bool
  | String      -> t
  | Typevar a   -> (try List.assoc a repl
                    with Not_found -> t)
  | Fun (l,e,r) -> Fun (subst repl l,e,subst repl r)
  | List u      -> List (subst repl u)

type unify_solution =
  | No_sol
  | Sol of (typevar * etype) list

exception No_solution

let rec unify_list = function
  | [] -> []
  | (l,r)::rest ->
    let sub = unify_list rest in
    (match subst sub l, subst sub r with
      | Unit, Unit           -> sub
      | Int, Int             -> sub
      | Bool, Bool           -> sub
      | String, String       -> sub
      | Typevar a, Typevar b -> if a = b then sub else (a,r)::sub
      | List a, List b       ->
	let sub' = unify_list [(a, b)] in
	sub' @ sub
      | Fun (a, _, b), Fun (c, _, d) ->
	let sub' = unify_list [(a, c);(b, d)] in
	sub' @ sub
      | (Typevar a, _)       -> if occurs a r then raise No_solution else (a,r)::sub
      | (_, Typevar a)       -> if occurs a l then raise No_solution else (a,l)::sub
      | (_,_ )
(*	| (Unit, _)
	| (Int, _)
	| (Bool, _)
	| (String, _)
	| (List _, _)
	| (Fun _, _) *)      -> raise No_solution)

let unify r t =
  try let res = unify_list [(r,t)] in
      Sol res
  with No_solution -> No_sol

(* determines whether the first arg is a generalization of the second *)
(* or framed differently: whether the second is a particular instance of the first *)
let rec types_compat t t' = match t,t' with
  | Unit,   Unit   -> true
  | Int,    Int    -> true
  | Bool,   Bool   -> true
  | String, String -> true
  | Fun (at,e,rt), Fun (at',e',rt') ->
    types_compat at' at && types_compat rt rt' && eff_leq e e'
  | List et, List et' -> types_compat et et'
  | Typevar a, _ ->
    (match unify t t' with
      | No_sol -> false
      | Sol _  -> true)
  | _, Typevar a -> false
  | _, _         -> false

let imm_eff t = match t with
  | Lit _
  | Variable (_,_)
  | Lambda (_,_,_,_)  -> no_eff
  | App (_,_,_,_,e)   -> e
  | Let (_,_,_,_,_,e) -> e
  | If (_,_,_,_,e)    -> e
    
let imm_type t =
  let rec lit_type l = match l with
    | LitUnit   -> Unit
    | LitInt _  -> Int
    | LitBool _ -> Bool
    | LitStr _  -> String
    | LitList l -> 
      let etyp =
	List.fold_left
	  (fun typacc elem ->
	    let etyp = lit_type elem in
	    if types_compat typacc etyp (* if typacc is a generalization of etyp *)
	    then etyp
	    else
	      if types_compat etyp typacc (* if etyp is a generalization of typeacc *)
	      then typacc
	      else
		failwith ("lit_type: elements in list literal disagree" ^
			     "  typacc is " ^ (typetostr ~effannot:true typacc) ^
			     "  etyp is " ^ (typetostr ~effannot:true etyp)))
	(Typevar (newtypevar())) l in
    List etyp in
  match t with
  | Lit l -> lit_type l
  | Variable (typ,_)    -> typ
  | Lambda (typ,_,_,_)  -> typ
  | App (typ,_,_,_,_)   -> typ
  | Let (_,_,_,_,typ,_) -> typ
  | If (typ,_,_,_,_)    -> typ
    

(** Enviroment type definitions and functions *)

module TypeSet = Set.Make(struct
                           type t = etype
			   let compare = Pervasives.compare
                         end)
module VarSet = Set.Make(struct
                           type t = variable
			   let compare = String.compare
                         end)
module VarMap = Map.Make(struct
                           type t = variable
			   let compare = String.compare
                         end)
module TypeMap = Map.Make(struct
                           type t = etype
			   let compare = Pervasives.compare
                          end)

let rec normalize_eff t = match t with
  | Typevar _
  | Unit
  | Int
  | Bool
  | String -> t
  | List t' -> let t'' = normalize_eff t' in
	       List t''
  | Fun (s,e,t) -> Fun (normalize_eff s,
			no_eff,
			normalize_eff t)
    
let addMultiMap key value map = (* assume key has been normalized *)
  try
    let s = TypeMap.find key map in
    let newSet = VarSet.add value s in
    TypeMap.add key newSet map
  with Not_found ->
    TypeMap.add key (VarSet.singleton value) map

let removeMultiMap key value map = (* assume key has been normalized *)
    let oldKeySet = TypeMap.find key map in
    let fixedOldTypeSet = VarSet.remove value oldKeySet in
    if VarSet.is_empty fixedOldTypeSet
    then TypeMap.remove key map
    else TypeMap.add key fixedOldTypeSet map

type tridirEnv = etype VarMap.t * (VarSet.t TypeMap.t) * (VarSet.t TypeMap.t)

let rec returnTypes = function
  | Fun (s, e, t) -> Fun (s, e, t) :: returnTypes t
  | t          -> [t]

let polyentry = Typevar (-1) (* special entry Typevar (-1) holds all vars with polymorphic return type *)
  
let addVar var newType (env, revEnv, returnEnv) =
  let env' = VarMap.add var newType env in (* Overrides existing entry *)

  let norm_newType = normalize_eff newType in

  let oldType = try Some (VarMap.find var env)
                with Not_found -> None in

  let revEnv' =
    let fixedRevEnv = match oldType with
      | Some oldType -> removeMultiMap (normalize_eff oldType) var revEnv
      | None         -> revEnv in
    addMultiMap norm_newType var fixedRevEnv in
    
  let returnEnv' =
    let rts = returnTypes norm_newType in
    let fixedReturnEnv = match oldType with
      | Some s ->
        List.fold_left (fun rEnv rt ->
	                  if ftv rt = [] (* return type polymorphic? Then syntactic comp. will not find it *)
	                  then removeMultiMap rt var rEnv
	                  else removeMultiMap polyentry var rEnv)
	  returnEnv (returnTypes (normalize_eff s))
      | None ->
        returnEnv in
    List.fold_left (fun rEnv rt ->
                      if ftv rt = [] (* return type polymorphic? Then syntactic comp. will not find it *)
		      then addMultiMap rt var rEnv
		      else addMultiMap polyentry var rEnv) fixedReturnEnv rts in
  (env', revEnv', returnEnv')

let lookupVar x (env, _, _) =
  try Some (VarMap.find x env) with Not_found -> None

let lookupType s (_, revEnv, _) =
  try TypeMap.find s revEnv
  with Not_found -> VarSet.empty

let lookupReturn s (env, _, returnEnv) =
  let concreteSet = try TypeMap.find s returnEnv
                    with Not_found -> VarSet.empty in
  let arity_s = arity s in
  let rec has_compat_rt t =
    (arity t = arity_s && types_compat t s) || (match t with
      | Fun (_,_,rt) -> has_compat_rt rt
      | _            -> false) in
  let polySet =
    VarSet.fold (fun x acc -> if has_compat_rt (VarMap.find x env) then VarSet.add x acc else acc)
      (try TypeMap.find polyentry returnEnv with Not_found -> VarSet.empty) VarSet.empty in
  VarSet.union concreteSet polySet


(** Generators *)

let a_code = int_of_char 'a'
let z_code = int_of_char 'z'
let alphaGen = Gen.map char_of_int (Gen.int_range a_code z_code)
let varGen = Gen.map (String.make 1) alphaGen

let stringGen = Gen.small_string ~gen:alphaGen
let stringToString s = "\"" ^ s ^ "\""

let sqrt i = int_of_float (sqrt (float_of_int i))

let arb_int =
  frequency
    [(10,small_int);
     (5,int);
     (1, oneofl [min_int;-1;0;1;max_int])]
  
let intGen = arb_int.gen (* Gen.small_int *)

(* Type-directed literal generator *)
let rec literalGen t eff size = match t with
  | Unit   -> Gen.return LitUnit
  | Int    -> Gen.map (fun i -> LitInt i) intGen
  | Bool   -> Gen.map (fun b -> LitBool b) Gen.bool
  | String -> Gen.map (fun s -> LitStr s) stringGen
  | List (Typevar _) -> Gen.return (LitList [])
  | List t ->
    if size = 0
    then Gen.return (LitList [])
    else Gen.map (fun ls -> LitList ls)
           (Gen.list_size (Gen.int_bound (sqrt size)) (literalGen t eff (sqrt size)))
      (*     (Gen.list_size (Gen.int_bound (size/2)) (literalGen t eff (size/2))) *)
  (* FIXME: - one element should/can have effect, if 'eff' allows *)
  (*        - list items should be able to contain arbitrary effectful exps *)
  | Typevar _ -> failwith "literalGen: typevar arg. should not happen"
  | Fun _  -> failwith "literalGen: funtype arg. should not happen"

let effGen = Gen.oneofl [(false,false); (true,false)]

let typeGen = (* Generates ground types (sans type variables) *)
  Gen.fix
    (fun recgen n ->
      if n = 0
      then Gen.oneofl [Unit;Int;Bool;String]
      else 
	Gen.frequency [
	(* Generate no alphas *)
	  (4,Gen.oneofl [Unit;Int;Bool;String]);
	  (1,Gen.map (fun t -> List t) (recgen (n/2)));
	  (1,Gen.map3 (fun t e t' -> Fun (t,e,t')) (recgen (n/2)) effGen (recgen (n/2)));
	])

(* Sized generator of variables according to the LIT rule 
   @param env  : surrounding environment
   @param s    : desired goal type
   @param eff  : desired effect
   @param size : size parameter

   --------------------- (LIT)
       env |- l : s
 *)
let litRules env s eff size =
  let rec listOfFun = function
    | List s -> listOfFun s
    | Fun _  -> true
    | _      -> false in
  match s with
    | List s when listOfFun s -> []
    | Unit   
    | Int    
    | Bool   
    | String
    | List _ -> [(6, Gen.map (fun l -> Some (Lit l)) (literalGen s eff size))]
    | Fun _
    | Typevar _ -> []

(* Sized generator of variables according to the VAR rule 
   @param env  : surrounding environment
   @param s    : desired goal type
   @param eff  : desired effect
   @param size : size parameter

      (t:s) \in env
   --------------------- (VAR)
       env |- t : s
 *)
let varRules env s eff size = (* vars have no immediate effect, so 'eff' param is ignored *)
  let candvars = VarSet.elements (lookupType (normalize_eff s) env) in
  let arity_s = arity s in
  let candvars' =
    List.filter (fun x -> match lookupVar x env with
                   | Some t -> (arity t = arity_s) && types_compat t s
		   | None -> failwith ("varRules: found variable " ^ x ^ " without associated type")) candvars in
  List.map (fun var -> (1, Gen.return (Some (Variable (s,var))))) candvars'


(* Sized generator of lambda terms according to the LAM rule 
   @param env  : surrounding environment
   @param u    : desired goal type
   @param eff  : desired effect
   @param size : size parameter

               (x:s), env |- m : t
    -------------------------------------------- (LAM)
       env |- (fun (x:s) -> m) : s -> t
 *)
let rec lamRules env u eff size = (* lams have no immediate effect, so 'eff' param is ignored *)
  let gen s eff t = Gen.(varGen >>= fun x ->
			 listPermuteTermGenOuter (addVar x s env) t eff (size/2) >>= function
			   | None   -> return None
			   | Some m ->
			     let myeff = imm_eff m in
			     return (Some (Lambda (Fun (s,myeff,imm_type m),x,s,m)))) in
  match u with
    | Unit | Int | Bool | String | List _
    | Typevar _    -> []
    | Fun (s,e,t) -> [(8, gen s e t)]


(* Sized generator of applications (calls) according to the APP rule 
   @param env  : surrounding environment
   @param t    : desired goal type
   @param eff  : desired effect
   @param size : size parameter

       env |- f : s -> t     env |- x : s
    ---------------------------------------- (APP)
                 env |- f x : t
 *)
and appRules env t eff size =
  let open Gen in
  let fromType funeff argeff s =
    listPermuteTermGenOuter env (Fun (s,eff,t)) funeff (size/2) >>=
      (function
        | None   -> Gen.return None
        | Some f ->
          listPermuteTermGenOuter env s argeff (size/2) >>=
            (function
              | None   -> Gen.return None
              | Some x ->
		match imm_type f with
		  | Fun (_,e,frange) ->
		    let funeff = imm_eff f in
		    let argeff = imm_eff x in
		    let ef,ev = eff_join e (eff_join funeff argeff) in
		    let eff' = (ef,ev || (fst funeff && fst argeff)) in
		    if eff_leq eff' eff
		    then Gen.return (Some (App (frange, f, imm_type x, x, eff')))
		    else (*Gen.return None*)
		      failwith "appRules generated application with too big effect"
		  | _ ->
		    failwith ("appRules generated application with non-function  "
			      ^ " t is " ^ (typetostr ~effannot:true t)
			      ^ " f is " ^ (toOCaml ~typeannot:false f)
			      ^ " imm_type f is " ^ (typetostr ~effannot:true (imm_type f))))) in
  (* May generate eff in either operator or operand *)
  [(4, typeGen (size/2) >>= fromType eff no_eff);
   (4, typeGen (size/2) >>= fromType no_eff eff)]
  

(* Sized generator of multi-argument applications (calls) according to the INDIR rule 
   @param env  : surrounding environment
   @param t    : desired goal type
   @param eff  : desired effect
   @param size : size parameter

     (f : rho1 -> ... -> rhon -> t) in env     env |- m1 : rho1  ...  env |- mn : rhon
   ------------------------------------------------------------------------------------- (INDIR)
                                  env |- f m1 ... mn : t
*)

and indirRules env t eff size =
    let mgu s t =
      let rec loop = function
        | []    -> None
        | r::rs -> match unify r t with
                    | Sol u  -> Some u
		    | No_sol -> loop rs in
      loop (returnTypes s) in

    let rec getArgTypes s t = match s with
      | s when (types_compat s t) -> []
      | Fun (s',_,t')   -> s' :: getArgTypes t' t 
      | s            -> failwith ("getArgTypes: should not happen  s is " ^ (typetostr ~effannot:true s)
				     ^ " t is " ^ (typetostr ~effannot:true t) ) in

    (* returns the index of the first effect - or else the number of arguments *)
    let rec first_eff = function
      | s when (types_compat s t || types_compat t s) -> 0
      | Fun (_,e,t)   -> if e = no_eff
     	                 then 1 + first_eff t
 	                 else 1
      | s            -> failwith ("first_eff: should not happen  " ^ (typetostr ~effannot:true s)) in
    
    (* recursively build application term argument by argument *)
    let rec apply term rType n effacc = function
      | []        -> Gen.return (Some term)
      | arg::args ->
	let myeff = if n=0 then eff else no_eff in (* arg 'n' may have effect 'eff' *)
        Gen.(>>=)
	  (listPermuteTermGenOuter env arg myeff (size/2))
          (function
            | None   -> Gen.return None
            | Some a ->
	      (match rType with
		| Fun (argType,funeff,newRType) ->
		  let my_actual_eff = eff_join funeff (imm_eff a) in (* actual effect *)
		  let effacc' = eff_join my_actual_eff effacc in
		  if eff_leq effacc' eff
		  then apply (App (newRType, term, imm_type a, a, effacc')) newRType (n-1) effacc' args
		  else
		    failwith "apply: overly effectful program generated. This should not happen."
		| _ -> failwith "apply: non-function type expecting argument. This should not happen")) in

    let application s f = (* s is the unnormalized, effect-full type *)
      let fTerm = Variable (s, f) in
      match mgu s t with
        | None   -> failwith ("indirRules, application: the return types of chosen variable "
			      ^ f ^ ":" ^ (typetostr ~effannot:true s)
			      ^ " do not match goal type " ^ (typetostr ~effannot:true t))
        | Some sub -> (* goal type and candidate unify with some subst *)
	  let goal_type = subst sub s in (* goal type may have free variables, which we need to inst. *)
	  let ftvs = ftv goal_type in
	  let rec build_subst vs = match vs with
	    | [] -> Gen.return []
	    | v::vs -> Gen.map2 (fun sub t -> (v,t)::sub) (build_subst vs) (typeGen (sqrt size)) in
	  Gen.(>>=) (build_subst ftvs) (fun sub' ->
	    let goal_type = subst sub' goal_type in
	    let argTypes = try getArgTypes goal_type (subst sub t)
                           with Failure exc ->
                             print_endline ("s is " ^ (typetostr ~effannot:true s));
                             print_endline ("sub is " ^ (Print.list (Print.pair
                                                                       (fun id -> "'a" ^ (string_of_int id))
                                                                       (typetostr ~effannot:true)) sub));
                             print_endline ("(subst sub s) is " ^ (typetostr ~effannot:true (subst sub s)));
                             print_endline ("t is " ^ (typetostr ~effannot:true t));
                             failwith exc in
	    let first_eff_index = first_eff goal_type in
 	    Gen.((if first_eff_index = 0
	          then return 0
      	          else int_bound (first_eff_index-1)) >>= fun n ->
   		    apply fTerm goal_type n no_eff argTypes))
    in

    let normalized_t = normalize_eff t in
    let suitableVariables = lookupReturn (normalized_t) env in (* this returns a set of cand. sans effects *)
    let f_type_map =
      let rec acc_rt_and_effect eff ty = (* check that all effects are leq eff and that rt is compat *)
	(if types_compat (normalize_eff ty) normalized_t (* (sans effects) types agree here *)
	 then types_compat ty t
	 else true)
	&& match ty with
	  | Fun (_,e,ty') -> eff_leq e eff && acc_rt_and_effect eff ty'
	  | _           -> true
      in
      (* there may be several variables with the same type *)
      (VarSet.fold
	 (fun f acc -> match lookupVar f env with
	   | Some ty ->
	     (match mgu ty t with (* some rt of receiver (sans effects) matches here *)
	       | None -> failwith "f_type_pairs: couldn't find a subst, which should not happen"
	       | Some sub ->
		 let ty' = subst sub ty in (* receiver may have poly type, which is too effectful when inst *)
		 if acc_rt_and_effect eff ty'
		 then addMultiMap ty f acc
		 else acc)
	   | None -> failwith "f_type_pairs: lookupVar failed, which should not happen")
	 suitableVariables TypeMap.empty) in
    TypeMap.fold (fun s fs acc -> (4, Gen.(>>=) (Gen.oneofl (VarSet.elements fs)) (application s))::acc) f_type_map []

(* Sized generator of let according to the LET rule 
   @param env  : surrounding environment
   @param t    : desired goal type
   @param eff  : desired effect
   @param size : size parameter

       env |- m : s     env, (x:s) |- n : t
    ------------------------------------------ (LET)
           env |- let x:s = m in n : t
 *)
and letRules env t eff size =
  let open Gen in
  let fromType s =
    varGen >>= fun x ->	
      listPermuteTermGenOuter env s eff (size/2) >>=
	(function
          | None   -> return None
          | Some m ->
            listPermuteTermGenOuter (addVar x s env) t eff (size/2) >>=
              (function
		| None   -> return None
		| Some n ->
		  let myeff = eff_join (imm_eff m) (imm_eff n) in
		  return (Some (Let (x,s,m,n,imm_type n,myeff))))) in
  [(6, typeGen (size/2) >>= fromType);]

and ifRules env t eff size =
  let open Gen in
  let gen =
    (listPermuteTermGenOuter env Bool eff (size/3)) >>= (function
      | None -> return None
      | Some b ->
	(listPermuteTermGenOuter env t eff (size/3)) >>= (function
	  | None -> return None
	  | Some m ->
	    let then_type = imm_type m in
	    (match unify then_type t with
	      | No_sol  -> failwith ("ifRules: generated type " ^ (typetostr ~effannot:true then_type)
				     ^ " in then branch does not unify with goal type " ^ (typetostr ~effannot:true t))
	      | Sol sub ->
		let subst_t = subst sub t in
		(listPermuteTermGenOuter env subst_t eff (size/3)) >>= (function
		  | None -> return None
		  | Some n ->
		    let else_type = imm_type n in
		    (match unify else_type subst_t with
			| No_sol  -> failwith ("ifRules: generated else branch type " ^ (typetostr ~effannot:true else_type)
 				           ^ " does not unify with subst goal type " ^ (typetostr ~effannot:true subst_t))
			| Sol sub' ->
			  let mytype = subst sub' subst_t in
			  let myeff = eff_join (imm_eff b) (eff_join (imm_eff m) (imm_eff n)) in
			  return (Some (If (mytype,b,m,n,myeff))))))))
 in
  [(3, gen)]

and listPermuteTermGenInner env goal size rules =
    let rec removeAt n xs = match (n, xs) with
      | (0, x::xs) -> xs
      | (n, x::xs) -> x :: removeAt (n - 1) xs
      | _          -> failwith "index out of bounds" in
    let elementsWeighted xs =
      let _,ig = List.fold_left
        (fun (i,acc) (w,g) -> (i+1,(w,Gen.pair (Gen.return i) g)::acc)) (0,[]) xs in
      Gen.frequency ig in
    let toTerm i = function
        | Some term -> Gen.return (Some term)
        | None      ->
          let remainingRules = removeAt i rules in
          listPermuteTermGenInner env goal size remainingRules in

    if rules = []
    then Gen.return None
    else Gen.(elementsWeighted rules >>= (fun (i,t) -> toTerm i t))

and listPermuteTermGenOuter env goal eff size =
    if size = 0
    then
      let rules = List.concat [litRules env goal eff size;
		               varRules env goal eff size; ] in
      listPermuteTermGenInner env goal size rules
    else
      let rules = List.concat [litRules env goal eff size;
			       (*varRules env goal eff size;*) (* var rule is covered by indir with no args *)
			       appRules env goal eff size;
			       lamRules env goal eff size;
			       indirRules env goal eff size;
			       letRules env goal eff size;
			       ifRules env goal eff size; ] in
    listPermuteTermGenInner env goal size rules

let listPermuteTermGenRecWrapper env goal eff =
    Gen.sized (listPermuteTermGenOuter env goal eff)


(** Initial environment *)

let initTriEnv =
  List.fold_left
    (fun acc (var, t) -> addVar var t acc)
    (VarMap.empty, TypeMap.empty, TypeMap.empty)
    [(* These follow the order and specification of the Pervasives module
	  https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html *)
     (* Comparisons *)
     ("(=)",            let a = Typevar (newtypevar()) in
			Fun (a, no_eff, Fun (a, (true,false), Bool)));
     ("(<>)",           let a = Typevar (newtypevar()) in
			Fun (a, no_eff, Fun (a, (true,false), Bool)));
     ("(<)",            Fun (Int, no_eff, Fun (Int, (true,false), Bool)));
     ("(>)",            Fun (Int, no_eff, Fun (Int, (true,false), Bool)));
(*   ("(<=)",           let a = Typevar (newtypevar()) in
			Fun (a, no_eff, Fun (a, (true,false), Bool)));
     ("(>=)",           let a = Typevar (newtypevar()) in
			Fun (a, no_eff, Fun (a, (true,false), Bool))); *)
     ("compare",        let a = Typevar (newtypevar()) in
			Fun (a, no_eff, Fun (a, (true,false), Int)));
(*   ("min",            let a = Typevar (newtypevar()) in
			Fun (a, no_eff, Fun (a, (true,false), a)));
     ("max",            let a = Typevar (newtypevar()) in
			Fun (a, no_eff, Fun (a, (true,false), a)));
     ("(==)",           let a = Typevar (newtypevar()) in
			Fun (a, no_eff, Fun (a, (true,false), Bool)));
     ("(!=)",           let a = Typevar (newtypevar()) in
			Fun (a, no_eff, Fun (a, (true,false), Bool))); *)
     (* Boolean operations *)
     ("not",            Fun (Bool, no_eff, Bool));
     ("(&&)",           Fun (Bool, no_eff, Fun (Bool, no_eff, Bool)));
     ("(||)",           Fun (Bool, no_eff, Fun (Bool, no_eff, Bool)));
     (* Integer arithmetic *)
(*   ("(~-)",           Fun (Int, no_eff, Int));
     ("(~+)",           Fun (Int, no_eff, Int)); *)
     ("succ",           Fun (Int, no_eff, Int));
     ("pred",           Fun (Int, no_eff, Int));
     ("(+)",            Fun (Int, no_eff, Fun (Int, no_eff, Int)));
     ("(-)",            Fun (Int, no_eff, Fun (Int, no_eff, Int)));
     ("( * )",          Fun (Int, no_eff, Fun (Int, no_eff, Int)));
     ("(/)",            Fun (Int, no_eff, Fun (Int, (true,false), Int)));
     ("(mod)",          Fun (Int, no_eff, Fun (Int, (true,false), Int)));
     ("abs",            Fun (Int, no_eff, Int));
(*   ("max_int",        Int);
     ("min_int",        Int); *)
     (* Bitwise operations *)
     ("(land)",         Fun (Int, no_eff, Fun (Int, no_eff, Int)));
     ("(lor)",          Fun (Int, no_eff, Fun (Int, no_eff, Int)));
     ("(lxor)",         Fun (Int, no_eff, Fun (Int, no_eff, Int)));
     ("lnot",           Fun (Int, no_eff, Int));
     ("(lsl)",          Fun (Int, no_eff, Fun (Int, no_eff, Int)));
     ("(lsr)",          Fun (Int, no_eff, Fun (Int, no_eff, Int)));
     ("(asr)",          Fun (Int, no_eff, Fun (Int, no_eff, Int)));
     (* Floating-point arithmetic *)
     (* String operations *)
     ("(^)",            Fun (String, no_eff, Fun (String, no_eff, String)));
     (* Character operations *)
     (* Unit operations *)
     ("ignore",         let a = Typevar (newtypevar()) in
			Fun (a, no_eff, Unit));
     (* String conversion functions *)
     ("string_of_bool", Fun (Bool, no_eff, String));
     ("bool_of_string", Fun (String, (true,false), Bool));
     ("string_of_int",  Fun (Int, no_eff, String));
     ("int_of_string",  Fun (String, (true,false), Int));
     (* Pair operations *)
     (* List operations *)
     ("(@)",            let a = Typevar (newtypevar()) in
			Fun (List a, no_eff, Fun (List a, no_eff, List a)));
     (* Input/output *)
     (* Output functions on standard output *)
     ("print_string",   Fun (String, (true,false), Unit));
     ("print_int",      Fun (Int, (true,false), Unit));
     ("print_endline",  Fun (String, (true,false), Unit));
     ("print_newline",  Fun (Unit, (true,false), Unit));
     (* Output functions on standard error *)
(*   ("prerr_string",   Fun (String, (true,false), Unit));
     ("prerr_int",      Fun (Int, (true,false), Unit));
     ("prerr_endline",  Fun (String, (true,false), Unit));
     ("prerr_newline",  Fun (Unit, (true,false), Unit));    *)
     (* Input functions on standard input *)
     (* General output functions *)
     (* General input functions *)
     (* Operations on large files *)
     (* References *)
     (* Result type *)
     (* Operations on format strings *)
     (* Program termination *)
     ("exit",           let a = Typevar (newtypevar()) in
			Fun (Int, (true,false), a));
(*   ("at_exit",        Fun (Fun (Unit, (true,false), Unit), (true,false), Unit)); *)
     (* More list operations from List module *)
     ("List.hd",        let a = Typevar (newtypevar()) in
			Fun (List a, (true,false), a));
     ("List.tl",        let a = Typevar (newtypevar()) in
			Fun (List a, (true,false), List a));
(*   ("List.concat",    let a = Typevar (newtypevar()) in
			Fun (List (List a), no_eff, List a)); *)
    ]


(** Shrinker and actual testing *)

let createLit t =
  let toTerm s = Some (Lit s) in
  match t with
    | Unit   -> toTerm LitUnit
    | Int    -> toTerm (LitInt (Gen.generate1 small_int.gen))
    | Bool   -> toTerm (LitBool (Gen.generate1 bool.gen))
    | String -> toTerm (LitStr (Gen.generate1 stringGen))
    | List _
    | Fun _
    | Typevar _ -> None

(* determines whether x occurs free (outside a binding) in the arg. exp *)
let rec fv x = function
  | Lit _             -> false
  | Variable (_,y)    -> x = y
  | Lambda (_,y,_,m ) -> if x = y then false else fv x m
  | App (_,m,_,n,_)   -> fv x m || fv x n
  | Let (y,_,m,n,_,_) -> fv x m || (if x = y then false else fv x n)
  | If (_,b,m,n,_)    -> fv x b || fv x m || fv x n

let rec shrinkLit = function
  | LitInt i   -> Iter.map (fun i' -> Lit (LitInt i')) (Shrink.int i)
  | LitStr s   -> Iter.map (fun s' -> Lit (LitStr s')) (Shrink.string s)
  | LitList ls -> Iter.map (fun ls' -> Lit (LitList ls')) (Shrink.list ls)
  | _          -> Iter.empty

let (<+>) = Iter.(<+>)
let rec termShrinker term = match term with
  | Lit l -> shrinkLit l
  | Variable (t,s) ->
    (match createLit t with
      | Some c  -> Iter.return c
      | _       -> Iter.empty)
  | Lambda (t,x,s,m) ->
    Iter.map (fun m' -> Lambda (t, x, s, m')) (termShrinker m)
  | App (rt,m,at,n,e)       ->
    (match createLit rt with
      | Some c -> Iter.return c
      | None   -> Iter.empty)
    <+> (if types_compat at rt then Iter.return n else Iter.empty)
    <+> (match m with
          | App (rt',m',at',n',e') when types_compat at' rt -> Iter.return n'
	  | Lambda (t',x,s,m') ->
	    if fv x m'
	    then Iter.return (Let (x,s,n,m',rt,e))
	    else Iter.of_list [m'; Let (x,s,n,m',rt,e)]
	  | Let (x,t,m',n',s,e') -> Iter.return (Let (x,t,m',App (rt,n',at,n,e),rt,e)) (* potential var capt.*)
	  | _ -> Iter.empty)
    <+> (Iter.map (fun m' -> App (rt,m',at,n,e)) (termShrinker m))
    <+> (Iter.map (fun n' -> App (rt,m,at,n',e)) (termShrinker n))
  | Let (x,t,m,n,s,e) ->
    (match createLit s with
      | Some c  -> Iter.return c
      | _       -> Iter.empty)
    <+>
    (match fv x n, m with
      | false, Let (x',t',m',_,_,_) -> Iter.of_list [n; Let (x',t',m',n,s,e)]  (* potential var capt.*)
      | false, _                    -> Iter.return n
      | true,  _ -> Iter.empty)
    <+> (Iter.map (fun m' -> Let (x,t,m',n,s,e)) (termShrinker m))
    <+> (Iter.map (fun n' -> Let (x,t,m,n',s,e)) (termShrinker n))
  | If (t,b,m,n,e) ->
    (match createLit t with
      | Some c  -> Iter.return c
      | _       -> Iter.empty)
    <+> (Iter.of_list [n; m])
    <+> (match b with
      | Lit _          -> Iter.empty
      | Variable (_,_) -> Iter.empty
      | _              -> let x = newvar () in
			  Iter.return (Let (x,Bool,b,If (t,Variable (Bool,x),m,n,e),t,e)))
    <+> (Iter.map (fun b' -> If (t,b',m,n,e)) (termShrinker b))
    <+> (Iter.map (fun m' -> If (t,b,m',n,e)) (termShrinker m))
    <+> (Iter.map (fun n' -> If (t,b,m,n',e)) (termShrinker n))
      
let wrapper shrinker opTerm = match opTerm with
  | None      -> Iter.empty
  | Some term -> Iter.map (fun t -> Some t) (shrinker term)

let shrinker term = wrapper termShrinker term

 (* 
          (t:s) \in env
   ---------------------------- (VAR)
       env |- t : s/ff/ff

              (x:s), env |- m : t/ef/ev
   ----------------------------------------------------- (LAM)
     env |- (fun (x:s) -> m) : s -ef/ev-> t/ff/ff

      env |- f : s -ef/ev-> t/fef/fev     env |- x : s/xef/xev
    ------------------------------------------------------------ (APP)
       env |- f x : t/ef or fef or xef/(fef and xef) || fev

      env |- m : s/mef/mev     env, (x:s) |- n : t/nef/nev
    -------------------------------------------------------- (LET)
        env |- let x:s = m in n : t/mef or nef/mev or nev

 *)


(* First version, checks type-and-effect annotation *)
let rec tcheck_lit l = match l with
  | LitUnit   -> Unit, no_eff
  | LitInt _  -> Int, no_eff
  | LitBool _ -> Bool, no_eff
  | LitStr _  -> String, no_eff
  | LitList l -> 
    let etyp =
      List.fold_left
	(fun typacc elem ->
	  let etyp,_ = tcheck_lit elem in
	  if types_compat typacc etyp (* if typacc is a generalization of etyp *)
	  then etyp
	  else
	    if types_compat etyp typacc (* if etyp is a generalization of typeacc *)
	    then typacc
	    else
	      failwith ("tcheck_lit: elements in list literal disagree" ^
			   "  typacc is " ^ (typetostr ~effannot:true typacc) ^
			   "  etyp is " ^ (typetostr ~effannot:true etyp)))
	(Typevar (newtypevar())) l in
    List etyp, no_eff

let rec tcheck env term = match term with
  | Lit l -> tcheck_lit l
  | Variable (t,v) ->
    (try
       let et = VarMap.find v env in
       if types_compat et t (* annotation may be more concrete then inferred type *)
       then et, no_eff
       else failwith ("tcheck: variable types disagree")
     with Not_found -> failwith "tcheck: unknown variable")
  | App (rt,m,at,n,ceff) ->
    let mtyp,meff = tcheck env m in
    let ntyp,neff = tcheck env n in
    (match mtyp with
      | Fun (at',e,rt') ->
	if meff = no_eff || neff = no_eff
	then
	  (match unify mtyp (Fun (at,ceff,rt)) with
	    | Sol sub ->
	      if types_compat (subst sub mtyp) (Fun (at,ceff,rt)) (* we obtain annot by instantiating inferred type *)
	      then
		(match unify ntyp at with
		  | Sol sub' ->
		    if types_compat (subst sub' ntyp) at (* we obtain annot by instantiating inferred type *)
		    then
		      let j_eff = eff_join e (eff_join meff neff) in
		      if eff_leq j_eff ceff
		      then rt,j_eff
		      else failwith ("tcheck: effect annotation disagree in application"
				     ^ "  ceff is " ^ (efftostr ceff)
				     ^ "  j_eff is " ^ (efftostr j_eff))
		    else
		      failwith ("tcheck: argument types disagree in application"
				^ "  ntyp is " ^ (typetostr ~effannot:true ntyp)
				^ "  at is " ^ (typetostr ~effannot:true at))
		  | No_sol ->
		    failwith ("tcheck: argument types do not unify in application"
			      ^ "  ntyp is " ^ (typetostr ~effannot:true ntyp)
			      ^ "  at is " ^ (typetostr ~effannot:true at)))
	      else failwith ("tcheck: function types disagree in application"
                             ^ ("  sub is " ^ (Print.list (Print.pair
                                                           (fun id -> "'a" ^ (string_of_int id))
                                                           (typetostr ~effannot:true)) sub))
			     ^ "  mtyp is " ^ (typetostr ~effannot:true mtyp)
			     ^ "  (Fun (at,ceff,rt)) is " ^ (typetostr ~effannot:true (Fun (at,ceff,rt))))
	    | No_sol ->
	      failwith ("tcheck: function types do not unify in application"
			^ "  mtyp is " ^ (typetostr ~effannot:true mtyp)
			^ "  (Fun (at,ceff,rt)) is " ^ (typetostr ~effannot:true (Fun (at,ceff,rt)))))
	else failwith "tcheck: application has subexprs with eff"
      | _ -> failwith "tcheck: application of non-function type")
  | Let (x,t,m,n,ltyp,leff) ->
      let mtyp,meff = tcheck env m in
      let ntyp,neff = tcheck (VarMap.add x mtyp env) n in
      if types_compat mtyp t (* annotation may be more concrete then inferred type *)
      then                   (*  annot "int list" instead of the more general "'a list" *)
	if types_compat ntyp ltyp
	then
	  let j_eff = eff_join meff neff in
	  if eff_leq j_eff leff 
	  then ntyp,leff
	  else failwith ("tcheck: let-effect disagrees with annotation"
			 ^ "  leff is " ^ (efftostr leff)
			 ^ "  j_eff is " ^ (efftostr j_eff))
	else failwith ("tcheck: let-body's type disagrees with annotation: " ^
	  "ntyp is " ^ (typetostr ~effannot:true ntyp) ^ "  ltyp is " ^ (typetostr ~effannot:true ltyp))
      else failwith "tcheck: let-bound type disagrees with annotation"
  | Lambda (t,x,s,m) ->
    let mtyp,meff = tcheck (VarMap.add x s env) m in
    let ftyp = Fun (s,meff,mtyp) in
    if types_compat ftyp t
    then ftyp, no_eff
    else failwith ("tcheck: Lambda's type disagrees with annotation: " ^
		      "ftyp is " ^ (typetostr ~effannot:true ftyp) ^ "  t is " ^ (typetostr ~effannot:true t))
  | If (t,b,m,n,e) ->
    let btyp,beff = tcheck env b in
    if btyp = Bool
    then
      if eff_leq beff e
      then
	let mtyp,meff = tcheck env m in
	let ntyp,neff = tcheck env n in
	(match unify mtyp ntyp with
	  | Sol sub ->
	    if types_compat (subst sub mtyp) t (* we obtain annot by instantiating inferred type *)
	    then
	      if types_compat (subst sub ntyp) t (* we obtain annot by instantiating inferred type *)
	      then
		if eff_leq meff e && eff_leq neff e
		then
		  let e' = eff_join beff (eff_join meff neff) in
		  t,e'
		else failwith ("tcheck: If's branch effects disagree with annotation")
	      else failwith ("tcheck: If's else branch type disagrees with annotation: "
			     ^ "  term is " ^ (toOCaml ~typeannot:false term)
			     ^ "  ntyp is " ^ (typetostr ~effannot:true ntyp)
			     ^ "  (subst sub ntyp) is " ^ (typetostr ~effannot:true (subst sub ntyp))
			     ^ "  t is " ^ (typetostr ~effannot:true t))
	    else failwith ("tcheck: If's then branch type disagrees with annotation: "
			   ^ "  term is " ^ (toOCaml ~typeannot:false term)
			   ^ "  mtyp is " ^ (typetostr ~effannot:true mtyp)
			   ^ "  (subst sub mtyp) is " ^ (typetostr ~effannot:true (subst sub mtyp))
			   ^ "  t is " ^ (typetostr ~effannot:true t))
	  | No_sol ->
	    failwith ("tcheck: If's branch types do not unify:  "
		      ^ "  term is " ^ (toOCaml ~typeannot:false term)
		      ^ "  mtyp is " ^ (typetostr ~effannot:true mtyp)
		      ^ "  ntyp is " ^ (typetostr ~effannot:true ntyp)))
      else failwith ("tcheck: If's condition effect disagrees with annotation")
    else failwith ("tcheck: If with non-Boolean condition")

let print_wrap t =
  Let ("i",Int,t,
       App (Unit,
	    Variable (Fun (Int,(true,false),Unit),
		      "print_int"),
	    Int,
	    Variable (Int,"i"),
	     (true,false)),
       Unit, (true,false))

let term_gen =
  make ~print:(Print.option (toOCaml ~typeannot:false))
(*       ~shrink:shrinker *)
    (listPermuteTermGenRecWrapper initTriEnv Int (true,false)) (* Initial goal here! *)

let term_gen_shrink = set_shrink shrinker term_gen

let typegen = make ~print:typetostr Gen.(frequency [(1, map (fun i -> Typevar i) (oneofl [1;2;3;4;5]));
						    (6, sized typeGen)])
let unify_funtest =
  Test.make ~count:1000 ~name:"unify functional"
    (pair typegen typegen) 
    (fun (ty,ty') -> match unify ty ty' with
      | No_sol    -> ty <> ty'
      | Sol t     -> let sty  = subst t ty in
		     let sty' = subst t ty' in
		     types_compat sty sty' || types_compat sty' sty)

let gen_classify =
  Test.make ~count:1000 ~name:"classify gen"
    (make ~collect:(fun t -> if t = None then "None" else "Some") term_gen.gen)
    (fun t -> let () = print_string "."; flush stdout in true)

let ocaml_test =
  Test.make ~count:500 ~name:"generated term passes OCaml's typecheck"
    term_gen_shrink
    (fun t_opt ->
      t_opt <> None ==>
	match t_opt with
	  | None   -> false
	  | Some t ->
	    try
	      let () = print_string "."; flush stdout in
	      let file = "testdir/ocamltest.ml" in
	      let () = write_prog (toOCaml t) file in
	      0 = Sys.command ("ocamlc -w -5@20-26 " ^ file)
	    with (Failure _) -> false)

let tcheck_test =
  Test.make ~count:500 ~name:"generated term type checks"
    term_gen(*_shrink*)
    (fun t_opt ->
      t_opt <> None ==>
	match t_opt with
	  | None   -> false
	  | Some t -> let env,_,_ = initTriEnv in
		      print_string "."; flush stdout;
		      match tcheck env t with
			  | Int,e -> eff_leq e (true,false)
			  | _,_ -> false)

let eq_test =
  Test.make ~count:500 ~name:"bytecode/native backends agree"
    term_gen_shrink
    (fun topt ->
      topt <> None ==>
	match topt with
	  | None -> false
	  | Some t ->
	    nativeByteEquivalence (toOCaml (print_wrap t)))

(* The actual call to QCheck_runner.run_tests_main is located in effmain.ml *)
