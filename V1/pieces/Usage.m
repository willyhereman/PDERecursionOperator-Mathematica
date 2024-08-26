(* Defines usage. *)
DefiningEquation::usage = 
"DefiningEquation[] brings up a menu of built in examples. \[NewLine]
DefiningEquation[eqn, opeartor, funcs, vars, opts] checks the 
recursion operator using its defining equation."

RecursionOperator::usage = 
"RecursionOperator[eqn, funcs, vars, param]."

BuildCandidate::FreeWeight = 
"There is freedom in the dilation symmetry `1` for this equation.  Please
set the free weight by using the option WeightRules.  For instance, if
Weight[u] isn't fixed, try WeightRules -> {Weight[u] -> 1}."

RecursionOperator::failed = 
"A valid recursion operator wasn't found.  Please try setting the Seed option
to an interger greater than one.  For example, Seed -> 2."

BuildCandidate::WeightsFail = 
"This system doesn't have a dilation symmetry, a requirement of this algorithm."

BuildCandidate::UnuniformWeights = 
"One of the terms in this system has terms of unequal rank.  The current version
of this package can't deal with this case."

RecursionOperator::eqns = 
"This package requires that the number of functions is equal to the number of
equations."

RecursionOperator::vars = 
"This package is only designed for two independent variable system, also
known as (1+1) dimensional systems."

RecursionOperator::dt = 
"This package is only designed to handle terms with at most one derivative
in `1`.  If you have a term like D[u[x,t], {t,2}], then rewrite it as
D[v[x,t], t] and add the equation D[u[x,t], t] == v[x,t]."

RecursionOperator::rational = 
"This package is only designed to work with polynomial systems with
fixed powers."