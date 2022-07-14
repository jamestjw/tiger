type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))



fun maxargs (s) = 
  case s of
       CompoundStm(stm1, stm2) => Int.max(maxargs(stm1), maxargs(stm2))
     | AssignStm(id, exp) => maxargs_exp(exp)
     (* Might be more nested expressions *)
     | PrintStm(exps) => Int.max(length(exps), maxargs_explist(exps))
and maxargs_exp(e) =
  case e of
       EseqExp(stm, _) => maxargs(stm)
     | _ => 0
and maxargs_explist(exps) =
  case exps of
       exp::rest => Int.max(maxargs_exp(exp), maxargs_explist(rest))
     | [] => 0
        
