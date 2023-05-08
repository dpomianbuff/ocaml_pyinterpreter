open Sexplib;;
open PyreAst.Concrete;;

exception RuntimeError of string;;
let errorf fmt = Printf.ksprintf (fun s -> raise (RuntimeError s)) fmt;;

let print_set set =
  Base.Hash_set.iter set ~f:(fun elem -> Printf.printf "%s\n" elem);;

 let parse_python content =
  let open PyreAst.Parser in
  with_context (fun context ->
  	match Concrete.parse_module ~context content with
  	| Result.Error _ ->
  	  let message = Format.sprintf "parsing error"
  	    (* Format.sprintf "Parsing error at line %d, column %d: %s" *)
  	    (* line column message *)
  	  in
  	  failwith message
  	| Result.Ok ast -> ast
  );;


let py_to_str x = 
  match x with
| Statement.Assign {location; targets; value; type_comment;} -> 
    let () = Printf.printf "Found an assignment at line: %s\n" (Sexp.to_string_hum(PyreAst.Concrete.Location.sexp_of_t location)) in
    let _ = List.iter (fun target -> Printf.printf "Target: %s\n" (Sexp.to_string_hum(Expression.sexp_of_t target))) targets in
    let _ = Printf.printf "value: %s" (Sexp.to_string_hum(Expression.sexp_of_t value)) in
    let _ = type_comment in 
    "Assignment\n"
| Statement.While { test : Expression.t; body : Statement.t list; orelse : Statement.t list } ->
  let _ = Printf.printf "test: %s" (Sexp.to_string_hum(Expression.sexp_of_t test)) in
  let _ = List.iter (fun target -> Printf.printf "body: %s\n" (Sexp.to_string_hum (Statement.sexp_of_t target))) body in
  ""
| Statement.FunctionDef { 
        location : Location.t;
        name : Identifier.t;
        args : Arguments.t;
        body : Statement.t list;
        decorator_list : Expression.t list;
        returns : Expression.t option;
        type_comment : string option;
} as x ->
  let _ = Printf.printf "function: %s\n" (Sexp.to_string_hum(Statement.sexp_of_t x)) in
  ""
| Statement.Expr { location : Location.t; value : Expression.t } as x ->
  let _ = Printf.printf "Statement Expression: %s\n" (Sexp.to_string_hum(Statement.sexp_of_t x)) in
  ""
| _ -> "unknown\n";;


let print_ast (ast:Module.t) : unit = 
  let rec foo aast (msg: string) = 
    let _ = Printf.printf "%s" msg in
    begin match aast with
    | [] -> Printf.printf "Done\n"
    | h::t -> foo t (py_to_str h)
  end in
  let _ = foo ast.body "Start\n" in ();;


open PyreAst.Concrete.Statement;;
open PyreAst.Concrete.Expression;;
open PyreAst.Concrete.Arguments;;

(* Helper functions *)

exception EnvError;;


module MyEnvironment = struct
  type env =
  | EmptyEnv
  | Extend of string * vtype * env
  and
  vtype =
    | NoneVal  
    | BoolVal of bool
    | IntVal of int
    | FloatVal of float
    | ListVal of vtype list
    | TupleVal of vtype list
    | StringVal of string
    | FunctionVal of fn
    and fn =
      { 
        args : Arguments.t
      ; env : env
      ; body : Statement.t list
      ; returns: Expression.t option
      } 

  exception Return_exn of vtype;;

  let const_to_vtype (c: Constant.t) =
    match c with
    | Constant.None -> NoneVal
    | Constant.False -> BoolVal false
    | Constant.True -> BoolVal true
    | Constant.Integer n -> IntVal n
    | Constant.Float n -> FloatVal n
    | Constant.String s -> StringVal s
    | _ -> failwith "not supported Constant"

  let rec lookup env x =
    match env with
    | EmptyEnv -> raise EnvError
    | Extend(identifier, value, env') ->
      if x = identifier then value else lookup env' x

  let rec vtype_to_string t =
    match t with
    | NoneVal -> "None"
    | BoolVal true -> "True"
    | BoolVal false -> "False"
    | IntVal i -> string_of_int i
    | FloatVal f -> string_of_float f
    | ListVal elems ->
        let rec loop acc elements =
        begin match elements with
            | [] -> acc ^ "]"
            | x::y::xs ->  loop (acc ^ (vtype_to_string x) ^ ",") (y::xs)
            | x::xs -> loop (acc ^ (vtype_to_string x))  xs
        end
        in loop "[" (List.rev elems)
    | TupleVal elems ->
        let rec loop acc elements =
        begin match elements with
            | [] -> acc ^ ")"
            | x::y::xs ->  loop (acc ^ (vtype_to_string x) ^ ",") (y::xs)
            | x::xs -> loop (acc ^ (vtype_to_string x))  xs
        end
        in loop "(" (List.rev elems)
    | StringVal s -> s
    | FunctionVal _ -> "<function>"


  let rec print_env (environ: env) = 
    match environ with
    | EmptyEnv -> Printf.printf "\n"
    | Extend(identifier, value, env') ->
      let () = Printf.printf "\n%s = %s"
        (identifier)
        (vtype_to_string value) in
      print_env env'
end;;

module Utils = struct
  let asrt = function
  | (true,_) -> ()
  | (false, str) -> failwith ("Assertion failure: "^str)

  let bool b = MyEnvironment.BoolVal b

  let make_string_from_identifier (v: Identifier.t) = Sexp.to_string_hum(Identifier.sexp_of_t v)

  let to_iterable v =
    match v with
    | MyEnvironment.TupleVal l -> l
    | MyEnvironment.ListVal l -> l
    | MyEnvironment.StringVal str -> 
      let rec loop i acc =
        if i < 0 then acc
        else loop (i - 1) ((MyEnvironment.StringVal(String.sub str i 1))::acc)
      in loop ((String.length str) - 1) []
    | o -> failwith "invalid iterable"

  let make_vtype_bool (vt: MyEnvironment.vtype) = 
    match vt with
    | BoolVal b -> b
    | IntVal i -> (i <> 0)
    | FloatVal f -> (f <> 0.)
    | ListVal l -> (List.length l) <> 0
    | TupleVal l -> (List.length l) <> 0
    | StringVal s -> (String.length s) <> 0
    | v -> failwith "input vtype cannot be interpreted as a bool"
end;;


let eval_binop (op: BinaryOperator.t) (left_value: MyEnvironment.vtype) (right_value: MyEnvironment.vtype) =
  match op with
  | BinaryOperator.Add -> 
    begin match (left_value, right_value) with
      | (MyEnvironment.IntVal a, MyEnvironment.IntVal b) -> MyEnvironment.IntVal (a+b)
      | (MyEnvironment.FloatVal a, MyEnvironment.IntVal b) -> MyEnvironment.FloatVal (a +. float_of_int b)
      | (MyEnvironment.IntVal a, MyEnvironment.FloatVal b) -> MyEnvironment.FloatVal (float_of_int a +. b)
      | (MyEnvironment.FloatVal a, MyEnvironment.FloatVal b) -> MyEnvironment.FloatVal (a +. b)
      | _ -> failwith "invalid operands type for addition"
    end
  | BinaryOperator.Sub ->
    begin match (left_value, right_value) with
      | (MyEnvironment.IntVal a, MyEnvironment.IntVal b) -> MyEnvironment.IntVal (a-b)
      | (MyEnvironment.FloatVal a, MyEnvironment.IntVal b) -> MyEnvironment.FloatVal (a -. float_of_int b)
      | (MyEnvironment.IntVal a, MyEnvironment.FloatVal b) -> MyEnvironment.FloatVal (float_of_int a -. b)
      | (MyEnvironment.FloatVal a, MyEnvironment.FloatVal b) -> MyEnvironment.FloatVal (a -. b)
      | _ -> failwith "invalid operands type for substraction"
    end
  | BinaryOperator.Mult ->
    begin match (left_value, right_value) with
      | (MyEnvironment.IntVal a, MyEnvironment.IntVal b) -> MyEnvironment.IntVal (a*b)
      | (MyEnvironment.FloatVal a, MyEnvironment.IntVal b) -> MyEnvironment.FloatVal (a *. float_of_int b)
      | (MyEnvironment.IntVal a, MyEnvironment.FloatVal b) -> MyEnvironment.FloatVal (float_of_int a *. b)
      | (MyEnvironment.FloatVal a, MyEnvironment.FloatVal b) -> MyEnvironment.FloatVal (a *. b)
      | _ -> failwith "invalid operands type for multiplication"
    end
  | BinaryOperator.Div ->
    begin match (left_value, right_value) with
      | (MyEnvironment.IntVal a, MyEnvironment.IntVal b) -> MyEnvironment.IntVal (a/b)
      | (MyEnvironment.FloatVal a, MyEnvironment.IntVal b) -> MyEnvironment.FloatVal (a /. float_of_int b)
      | (MyEnvironment.IntVal a, MyEnvironment.FloatVal b) -> MyEnvironment.FloatVal (float_of_int a /. b)
      | (MyEnvironment.FloatVal a, MyEnvironment.FloatVal b) -> MyEnvironment.FloatVal (a /. b)
      | _ -> failwith "invalid operands type for division"
    end
  | BinaryOperator.Mod ->
    begin match (left_value, right_value) with
      | (MyEnvironment.IntVal a, MyEnvironment.IntVal b) -> MyEnvironment.IntVal (a mod b)
      | _ -> failwith "invalid operands type for modulo"
    end
  | BinaryOperator.Pow ->
    begin match (left_value, right_value) with
      | (MyEnvironment.IntVal a, MyEnvironment.IntVal b) -> MyEnvironment.FloatVal (float_of_int a ** float_of_int b) 
      | (MyEnvironment.FloatVal a, MyEnvironment.IntVal b) -> MyEnvironment.FloatVal (a ** float_of_int b)
      | (MyEnvironment.IntVal a, MyEnvironment.FloatVal b) -> MyEnvironment.FloatVal (float_of_int a ** b)
      | (MyEnvironment.FloatVal a, MyEnvironment.FloatVal b) -> MyEnvironment.FloatVal (a ** b)
      | _ -> failwith "invalid operands type for division"
    end
  | _ -> failwith "only add is implemented";;


let rec eval_expression (environ: MyEnvironment.env) = function
  | Constant {value : Constant.t; _} -> 
      MyEnvironment.const_to_vtype value
  | Name {id : Identifier.t; _} -> 
      let found = MyEnvironment.lookup environ (Utils.make_string_from_identifier id) in
      found
  | BinOp { left : Expression.t; op : BinaryOperator.t; right : Expression.t; _} ->
    let left_value = eval_expression environ left in
    let right_value = eval_expression environ right in
    eval_binop op left_value right_value
  | BoolOp { op = And; values: Expression.t list; _ } ->
    Utils.bool (List.for_all (fun v -> eval_expression environ v |> Utils.make_vtype_bool) values)
  | BoolOp { op = Or; values: Expression.t list; _ } ->
    Utils.bool (List.exists (fun v -> eval_expression environ v |> Utils.make_vtype_bool) values)
  | Compare { left : Expression.t; ops : ComparisonOperator.t list; comparators : Expression.t list; _ } ->
    let comb = List.combine ops comparators in
    let rec loop left ops_comp_list = 
      begin match ops_comp_list with
      | [] -> MyEnvironment.BoolVal true
      | (o, e)::rest -> 
        let left_val = eval_expression environ left in
        let right_val = eval_expression environ e in
        let cmp_res = 
          begin match (left_val, right_val) with
            | (lval, rval) -> 
              begin match o with
                | ComparisonOperator.Eq -> lval = rval
                | ComparisonOperator.NotEq -> lval <> rval
                | ComparisonOperator.Lt -> lval < rval
                | ComparisonOperator.Lte -> lval <= rval
                | ComparisonOperator.Gt -> lval > rval
                | ComparisonOperator.Gte -> lval >= rval
                | ComparisonOperator.In -> List.mem lval (Utils.to_iterable rval)
                | ComparisonOperator.NotIn -> not (List.mem lval (Utils.to_iterable rval))
                | _ -> failwith "unimplemented comparison operator"
              end
          end
          in if cmp_res then loop e rest else (Utils.bool cmp_res)
      end
    in
    loop left comb

  | Call { func : Expression.t; args : Expression.t list; _ } ->
    let func = eval_expression environ func in
    let call_arg_list = args in
    begin match func with
      | MyEnvironment.FunctionVal { args: Arguments.t; env: MyEnvironment.env; body: Statement.t list; } ->
        let func_env = env in
        let func_args = args.args in
        (* eval args *)
        let rec loop1 result argument_list =
          begin match argument_list with
            | [] -> result
            | x::xs -> loop1 ((eval_expression func_env x)::result) xs
          end in
        let eval_call_arg_list = loop1 [] call_arg_list in
        let eval_args_func_args = List.combine func_args eval_call_arg_list in
        (* compute assignments and build new function env *)
        let rec loop_assign func_env lst =
          begin match lst with
            | [] -> func_env
            | (target, vvalue)::xs ->
              loop_assign (eval_assign_arg func_env target vvalue) xs
          end in
        let new_func_env = loop_assign func_env eval_args_func_args in
        (try
          let _ = eval_statements new_func_env body in
          MyEnvironment.NoneVal
        with
        | MyEnvironment.Return_exn ret_value -> ret_value
        )
      | _ -> failwith "unknown type of function"
    end
  | Expression.List {elts: Expression.t list; _} -> 
    let rec loop (result: MyEnvironment.vtype list) (elts: Expression.t list) =
      match elts with
      | [] -> MyEnvironment.ListVal result
      | x::xs -> 
        let eval_result = eval_expression environ x in
        loop (eval_result::result) xs
    in loop [] elts
  | Expression.Tuple {elts: Expression.t list; _} -> 
    let rec loop (result: MyEnvironment.vtype list) (elts: Expression.t list) =
      match elts with
      | [] -> MyEnvironment.TupleVal result
      | x::xs -> 
        let eval_result = eval_expression environ x in
        loop (eval_result::result) xs
    in loop [] elts

  | _ -> failwith "Unsupported expression"

 and eval_statement (environ: MyEnvironment.env) = function
  | FunctionDef { name:Identifier.t; args:Arguments.t; body:Statement.t list; returns:Expression.t option } ->
    let function_name = Utils.make_string_from_identifier name in 
    let env = environ in
    let function_val = MyEnvironment.FunctionVal {args; env; body; returns} in
    MyEnvironment.Extend (function_name, function_val, environ)
  | Return { value : Expression.t option; _ } ->
    begin match value with
     | None -> raise (MyEnvironment.Return_exn MyEnvironment.NoneVal)
     | Some (exp:Expression.t) -> 
        (let evaluated_exp = eval_expression environ exp in
        raise (MyEnvironment.Return_exn evaluated_exp) : MyEnvironment.env)
    end
  | Expr { value } -> let _ = eval_expression environ value in environ
  | Assign {targets:Expression.t list; value; _} ->
    let rec loop targets loop_env =
      match targets with
      | [] -> loop_env
      | target::xs -> 
        let exp_val = eval_expression loop_env value in
        let new_env = eval_assign loop_env target exp_val in
        loop xs new_env in
    loop targets environ
  | AugAssign { target; op; value } ->
    let value = eval_expression environ value in
    let value = eval_binop op (eval_expression environ target) value in
    eval_assign environ target value
  | If { test : Expression.t; body; orelse; _} ->
    let test_val = eval_expression environ test in
    begin match test_val with
      | BoolVal vb -> if vb then eval_statements environ body else eval_statements environ orelse
      | _ -> failwith "invalid if condition"
    end
  | For { target; iter; body; orelse } ->
    let iter = eval_expression environ iter |> Utils.to_iterable in
    let rec loop2 (iterable:MyEnvironment.vtype list) loop_env = 
      begin match iterable with
      | [] -> loop_env
      | x::xs -> 
        let new_env = eval_assign loop_env target x in
        let env' = eval_statements new_env body in
        loop2 xs env'
      end
    in 
    loop2 iter environ
  | While { test; body; orelse } ->
    let rec loop loop_env =
      (* let _ = Printf.printf "test expression: %s" (Sexp.to_string_hum(Expression.sexp_of_t test)) in *)
      (* let _ = Printf.printf "Environment:\n" in *)
      (* let _ = MyEnvironment.print_env loop_env in *)
      let test_cond = eval_expression loop_env test in
      let bool_result = Utils.make_vtype_bool test_cond in
      (* let _ = Printf.printf "%s" (MyEnvironment.vtype_to_string test_cond) in *)
      if bool_result 
      then (
          let new_env = eval_statements loop_env body in
          loop new_env
      )
      else eval_statements loop_env orelse
    in
    loop environ 
  | _ -> failwith "statement not implemented yet"

and
 eval_statements (environ:MyEnvironment.env) = function
  | [] -> environ
  | head::tail ->
      let new_environ = eval_statement environ head in
      eval_statements new_environ tail
and 
eval_assign (environ:MyEnvironment.env) (target:Expression.t) (vtype_value: MyEnvironment.vtype) =
  let new_environ = 
    match target with
    | Name {id: Identifier.t; ctx: ExpressionContext.t; _} -> 
       begin match ctx with
       | Store -> MyEnvironment.Extend ((Utils.make_string_from_identifier id), vtype_value, environ)
       | _ -> environ
      end
    | Tuple {elts : Expression.t list; ctx = ExpressionContext.Store; _} -> 
      begin match vtype_value with
        | MyEnvironment.TupleVal tupleValList ->
          let comb = List.combine elts (List.rev tupleValList) in
          let rec loop lst loop_env =
            begin match lst with
              | [] -> loop_env
              | (vexp, vval)::tl -> 
                let new_env = eval_assign loop_env vexp vval in
                loop tl new_env
            end
          in loop comb environ
        | _ -> environ
      end
    | _ -> environ
  in new_environ
and 
eval_assign_arg (environ:MyEnvironment.env) (target:Argument.t) (vtype_value: MyEnvironment.vtype) =
  MyEnvironment.Extend ((Utils.make_string_from_identifier target.identifier), vtype_value, environ)
;;

(* --- TESTS --- *)
let run_tests () =
  let ut (python_code: string) (to_check: (string*MyEnvironment.vtype) list) (error_message: string) =
    let pyast = parse_python python_code in
    let rec loop = function
      | [] -> ()
      | (vname, vvalue)::xs ->
          let env = eval_statements MyEnvironment.EmptyEnv pyast.body in
          let _ = Utils.asrt(MyEnvironment.lookup env vname = vvalue, error_message) in
          loop xs
      in loop to_check
  in
  let _ = ut "x=3" [("x", (MyEnvironment.IntVal 3))] "Int constant assignment failed" in
  let _ = ut "x=True" [("x",(MyEnvironment.BoolVal true))] "Bool true constant assignment failed" in
  let _ = ut "x=False" [("x",(MyEnvironment.BoolVal false))] "Bool false constant assignment failed" in
  let _ = ut "x=2+3" [("x",(MyEnvironment.IntVal 5))] "Int arithmetic add failed" in
  let _ = ut "x=2-3" [("x",(MyEnvironment.IntVal (-1)))] "Int arithmetic sub failed" in
  let _ = ut "x=4*2" [("x",(MyEnvironment.IntVal 8))] "Int arithmetic mult failed" in
  let _ = ut "x=4/2" [("x",(MyEnvironment.IntVal 2))] "Int arithmetic div failed" in
  let _ = ut "x=4/2+2*7" [("x",(MyEnvironment.IntVal 16))] "Int arithmetic expression failed" in
  let _ = ut "x=16/(2+2*7)" [("x",(MyEnvironment.IntVal 1))] "Int arithmetic parens expression failed" in
  let _ = ut "x = 3 < 4" [("x", (MyEnvironment.BoolVal true))] "Lt comparator failed" in
  let _ = ut "x = 3 <= 4" [("x", (MyEnvironment.BoolVal true))] "Lte comparator failed" in
  let _ = ut "x = 3 == 4" [("x", (MyEnvironment.BoolVal false))] "Eq comparator failed" in
  let _ = ut "x = 3 > 4" [("x", (MyEnvironment.BoolVal false))] "Gt comparator failed" in
  let _ = ut "x = 3 >= 4" [("x", (MyEnvironment.BoolVal false))] "Gte comparator failed" in
  let _ = ut "x = 2**10" [("x", (MyEnvironment.FloatVal 1024.))] "Power operator failed" in
  let pc1 =
"
x = 15
y = x/15
" in
  let _ = ut pc1 [("y",(MyEnvironment.IntVal 1))] "Int arithmetic variable expression failed" in
  let pc2 =
"
x = 10
x += 10
x -= 5
x *= 2
x /= 6
" in
  let _ = ut pc2 [("x",(MyEnvironment.IntVal 5))] "Augument assigned failed" in
  let ite1 =
"
x = 3
if x==(2+1):
  yi=2
elif x==1:
  yi=1
else:
  yi=0" in
  let _ = ut ite1 [("yi",(MyEnvironment.IntVal 2))] "ITE1 failed" in
  let ite2 =
"
x = 7
if x==(2+1):
  yi=2
elif x==1:
  yi=1
else:
  yi=0" in
  let _ = ut ite2 [("yi",(MyEnvironment.IntVal 0))] "ITE1 failed" in
  let for_pycode = 
"
x = 'a'
for c in 'abcd':
  x = c
" in
  let _ = ut for_pycode [("x", (MyEnvironment.StringVal "d")); ("c", (MyEnvironment.StringVal "d"))] "asd" in
  let list_pycode = 
"
x = [True, False, 2+3]
" in
  let expected = (MyEnvironment.ListVal (List.rev [MyEnvironment.BoolVal true; MyEnvironment.BoolVal false; MyEnvironment.IntVal 5])) in
  let _ = ut list_pycode [("x", expected)] "List assignment failed" in
  let tuple_pycode = 
"
x = (True, False, 2+3)
" in
  let expected = (MyEnvironment.TupleVal (List.rev [MyEnvironment.BoolVal true; MyEnvironment.BoolVal false; MyEnvironment.IntVal 5])) in
  let _ = ut tuple_pycode [("x", expected)] "Tuple type assignment failed" in
  let tuple_assignment = 
"
a, b = 0, 1
" in
  let _ = ut tuple_assignment [("a", (MyEnvironment.IntVal 0)); ("b", (MyEnvironment.IntVal 1))] "Tuple values assignment failed" in
  let fibo_pycode =
"
a, b, count, n = 1, 1, 0, 3
while count < n:
  a, b = b, a+b
  count += 1
" in
  let _ = ut fibo_pycode [("a", (MyEnvironment.IntVal 3))] "Fibo While failed" in
  let fibo_func_pycode = 
"
def fibo(n):
  a, b, count = 0, 1, 0
  while count < n:
    a, b = b, a+b
    count += 1
  return b
result = fibo(7)
" in
  let _ = ut fibo_func_pycode [("result", (MyEnvironment.IntVal 21))] "Fibo Function failed" in
  let factorial_func_pycode = 
"
def fact(n):
  count, result = 0, 1
  while count<n:
    count+=1
    result *= count
  return result

f = fact(4)
" in
  let _ = ut factorial_func_pycode [("f", (MyEnvironment.IntVal 24))] "Factorial Function failed" in
  let in_pycode =
"
x = 3
result = x in [1,2,3]
" in
  let _ = ut in_pycode [("result", (MyEnvironment.BoolVal true))] "\"In\" Compare expression failed" in
  let not_in_pycode =
"
x = 3
result = x not in [1,2,3]
" in
  let _ = ut not_in_pycode [("result", (MyEnvironment.BoolVal false))] "\"Not In\" Compare expression failed" in
  ();;
(* --- END TESTS --- *)


(* ---- Main --- *)
let usage () = Printf.printf "Usage: %s <interpret | test> [input_file]\n" Sys.argv.(0) in
let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
in
let main = fun () -> 
  begin match Array.length Sys.argv with
    | 2 -> 
      begin match Sys.argv.(1) with
        | "test" -> run_tests ()
        | _ -> usage ()
      end
    | 3 -> 
      begin match Sys.argv.(1) with
        | "interpret" ->
          let file_name = Sys.argv.(2) in
          let pycode = read_file file_name in
          let pyast = parse_python pycode in
          let env = eval_statements MyEnvironment.EmptyEnv pyast.body in
          let _ = MyEnvironment.print_env env in
          ()
        | _ -> usage ()
      end
    | _ -> usage () 
  end
in
main ();;
(* -------------------------*)
      



(* let pycode = 
"
x = 3
result = x in [1,2,3]
" in
let pyast = parse_python pycode in
(* let _ = print_ast pyast in *)
let env = eval_statements MyEnvironment.EmptyEnv pyast.body in
let _ = MyEnvironment.print_env env in
let a = MyEnvironment.lookup env "result" in
let _ = Printf.printf "\n%s\n" (MyEnvironment.vtype_to_string a) in
();; *)
