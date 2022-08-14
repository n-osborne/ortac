open Ortac_core
module W = Warnings
module S = Map.Make (String)
open Ppxlib
module Ident = Gospel.Identifier.Ident


(*Respresentation that comes after Drv.t

Gospel TAST = Ast1
Drv.t = Ast2
Ast3 = Ast3

phase1: Ast1 -> Ast2
phase2 : Ast2 -> Ast3
phase3: Ast3 -> final product
*)

(*not allowing tuples because stm has no 'a ty_show for tuples*)
type typ =
  | Int
  | Integer
  | String
  | Bool
  | Unit
  | List of typ

let get_typ_args t =
  match t with
  | List a -> [a]
  | Int | Integer | String | Bool| Unit -> []


type ocaml_var = {name : string; label : arg_label; typ: typ}


(*
fn name -> {name of the special first argument which is always of type t;
           remaining arguments;
           returns;
           whether it is pure} *)
type cmd_ele = {targ_name: string; args: ocaml_var list; ret: ocaml_var list; pure:bool}
type cmd= cmd_ele S.t

(* model name -> model type 
to be turned into a a record with field names taken from the models of type t*)
type state = typ S.t

(*fn name -> list of Qcheck generators for args*)
type arb_cmd = (expression list) S.t

(* maps each state field name to its initial value
determined by the post conditions of special function init_sut*)
type init_state = expression S.t


(*cmd constructor -> {requires and checks; state field name -> new value}
*)
type next_state_case = { pres: expression list; next: expression S.t} 
type next_state = next_state_case S.t 

(*command name -> {what it returns, is it pure} *)
type run = (ocaml_var list * bool) S.t


(*cmd_constr -> all its requires*)
type precond = expression list S.t 


type xpost = {
  name : string;
  args : int;
  translation : cases; 
}

type postcond_case =
  {
   checks: expression list;
   raises: xpost list; 
   ensures: expression list; }

type postcond = postcond_case S.t

type stm = {module_name : string;
            cmd: cmd;
            state : state;
            arb_cmd : arb_cmd;
            init_state: init_state;
            next_state: next_state;
            run: run;
            precond : precond;
            postcond: postcond
           }


(*assumptions made by this tool:
  1. the system under test type is called 't' in the mli file
  2. all of the testable functions in the file have types of the form
  t -> basic_type ... -> basic_type where basic_type is a type
  with a generator in QCheck
  3. there is a function init_sut : () -> t where all of the fields of the return value
  are specified
  4. none of the conditions can refer to sut : t directly.
  they can only refer to the FIELDS of sut. start here need to actually check this.
  5. the specification of the value of a field in the next state is written with the state field on the LHS.  start here dont actually need this.
*)
