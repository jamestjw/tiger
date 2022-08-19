(* The IR of this compiler is simple expression trees *)

open Temp

module Tree = struct
  type exp =
    | CONST of int
    (* Corresponds to an assembly language label *)
    | NAME of Temp.label
    (* A temp in the abstract machine is similar to a register in a
       real machine. However, the abstract machine has an infinite
       number of temporaries.*)
    | TEMP of Temp.temp
    | BINOP of binop * exp * exp
    (* Treats the exp as a memory address, can be used as both
       a l-value and r-value. *)
    | MEM of exp
    (* CALL(f, l)
        A procedure call: the application of function f to argument list l.
        The subexpression l is evaluated before the arguments which are
        evaluated left to right. *)
    | CALL of exp * exp list
    (* The statement is evaluated for side effects, then the expression
       is evaluated for a result. *)
    | ESEQ of stm * exp

  and stm =
    (* MOVE (TEMP t, e) Evaluate e and move it into temporary t.
       MOVE(MEM(e1), e2) Evaluate e1 yielding address a. Then evaluate e2, and
       store the result into wordSize bytes of memory starting at a. *)
    | MOVE of exp * exp
    (* Evaluates the expression and discards the result *)
    | EXP of exp
    (* JUMP(e, labs) Jump to address e. The destionation e may be a literal label, e.g.
       NAME(lab), or an address calculated by other expressions. The list of labels
       specifies all the possible locations that the expression e can evaluate to; this
       is necessary for dataflow analysis later.
       To jump to a known label: JUMP(NAME l, [l]) *)
    | JUMP of exp * Temp.label list
    (* Evaluate e1, e2 in that order, yielding values a,b. Then compare a, b using the
       relational operator o. If the result is true, jump to t; otherwise jump to f. *)
    | CJUMP of relop * exp * exp * Temp.label * Temp.label
    (* The statement s1 followed by s2 *)
    | SEQ of stm * stm
    (* LABEL(n) Define the constant value of name n to be the current machine code
       address. This is like a label definition in assembly language. After which
       NAME(n) may be the target of jumps, calls, etc *)
    | LABEL of Temp.label

  and binop =
    (* Integer arithmetic operators *)
    | PLUS
    | MINUS
    | MUL
    | DIV
    (* Integer bitwise logical operators *)
    | AND
    | OR
    | XOR
    (* Integer logical shift operators *)
    | LSHIFT
    | RSHIFT
    (* Integer arithmetic right-shift *)
    | ARSHIFT

  and relop =
    (* Integer equality and nonequality (signed and unsigned) *)
    | EQ
    | NE
    (* Signed integer inequalities *)
    | LT
    | GT
    | LE
    | GE
    (* Unsigned integer inequalities *)
    | ULT
    | ULE
    | UGT
    | UGE
end
