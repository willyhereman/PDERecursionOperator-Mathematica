(* : Title : The Defining Equation *)
(* : Author : Douglas Baldwin *)
(* : Date : Wednesday, July 03, 2002 *)

(* Starts package environment. *)
BeginPackage["recursionOperator`"]

<< Usage.m;

(* Begins private environment. *)
Begin["`Private`"]

<< Menu.m  (* no free constants *)
(* << MenuFree.m  *) (* with free constants *)

<< MainFunction.m

<< operatorTimes.m

<< operatorExpand.m

<< operatorConstraints.m

<< OperatorConverter.m

<< FrechetDerivativeOfSystem.m

<< operatorFrechetDerivative.m

<< OperatorExpressionMultiply.m

<< CleanUpFunction.m

End[] (* Ends `Private` *)

SetAttributes[DefiningEquation, ReadProtected];

EndPackage[]