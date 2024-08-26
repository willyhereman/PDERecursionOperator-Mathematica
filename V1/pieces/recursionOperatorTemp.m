(* : Title : The Defining Equation *)
(* : Author : Douglas Baldwin *)
(* : Date : Saturday, January 11, 2003 *)

(* Starts package environment. *)
(* BeginPackage["recursionOperator`"] *)

<< Usage.m;

(* Begins private environment. *)
(* Begin["`Private`"] *)

(* Added on 1/11/2003 to remove message Solve::svars when using *)
(* operatorConstraints with constants.  Code from Unal Goktas.  *)
If[$VersionNumber < 4,
   Attributes[Internal`DeactivateMessages] = {HoldAll};
   Internal`DeactivateMessages[body_, msgs___MessageName] :=
      Module[{wasOn = Select[Hold[msgs], Head[#] =!= $Off &], result},
         CheckAbort[
            Scan[Off, wasOn];
            result = body;
            Scan[On, wasOn];
            result,
            Scan[On, wasOn];
            Abort[]
         ]
      ]
]

(* << Menu.m *)
<< MenuFree.m

<< MainFunction.m

<< operatorTimes.m

<< operatorExpand.m

<< operatorConstraints.m

<< OperatorConverter.m

<< FrechetDerivativeOfSystem.m

<< operatorFrechetDerivative.m

<< OperatorExpressionMultiply.m

<< CleanUpFunctionTemp.m

(* End[] *)(* Ends `Private` *)

SetAttributes[DefiningEquation, ReadProtected];

(* EndPackage[] *)