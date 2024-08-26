(* : Title : The Defining Equation *)
(* : Author : Douglas Baldwin *)
(* : Date : Saturday, January 11, 2003 *)

<< InvariantsSymmetries.m;

(* Starts package environment. *)
BeginPackage["recursionOperator`"]

<< Usage.m;

Options[RecursionOperator] =
  { Seed -> 1, UnknownCoefficients -> C}

(* Begins private environment. *)
Begin["`Private`"]

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

<< BuildCandidate.m

(* << Menu.m  *)
<< MenuFree.m 

<< MainFunction.m

<< operatorTimes.m

<< operatorExpand.m

<< operatorConstraints.m

<< OperatorConverter.m

<< FrechetDerivativeOfSystem.m

<< operatorFrechetDerivative.m

<< OperatorExpressionMultiply.m

<< CleanUpFunction.m

(* Takes the input and puts it in the correct form *)
RecursionOperator[equations_, funcs_, vars_, param_List:{},
  options___? OptionQ] :=
  RecursionOperator[
    If[ Head[#] === List, #, {#}]&[equations], 
    If[ Head[#] === List, #, {#}]&[funcs], 
    If[ Head[#] === List, #, {#}]&[vars],
    If[ Head[#] === List, #, {#}]&[param],
    options
  ] /; ! And[ Head[equations] === List, Head[funcs] === List,
    Head[vars] === List, Head[param] === List];

(* Generates a candidate form and then tests that form. *)
RecursionOperator[equations_List, funcs_List, 
    vars_List, param_List:{}, options___? OptionQ] :=
  Module[{ recursionOperatorDebug = True,
           candidate,
           solution
         }, (* Protected Local Variables *)

    (* Sets two global variables for funcs and vars. *)
    RecursionOperator`UserFunctions = funcs;
    RecursionOperator`UserVariables = vars;

    candidate = 
      BuildCandidate[equations /. Equal[a_,b_] :> a-b, 
        funcs, vars, param, options];
    
    If[recursionOperatorDebug,
      Print["The candidate recursion operator is:"];
      Print[CleanUpFunction[ candidate ]];
    ];

    solution = 
      First /@ 
      DefiningEquation[equations /. Equal[a_,b_] :> b,
        candidate, funcs, vars, param
      ][[2]];

    If[recursionOperatorDebug,
      Print["The candidate after being run throught the defining equtaion:"];
      Print[CleanUpFunction[ solution ]];
    ];

    If[ Union[Flatten[solution]] === {0},
      Message[RecursionOperator::failed];
      Abort[];
    ];      

    If[
      Or @@ 
      ( First /@
        ( DefiningEquation[equations /. Equal[a_,b_] :> b,
            #, funcs, vars, param
          ]& /@ solution ) ),
      Return[    
        ToExpression[ 
          StringReplace[
            ToString[
              InputForm[solution]
            ],
            "recursionOperator`Private`" -> ""
          ]
        ] 
      ],
      Message[RecursionOperator::failed];
      Abort[];
    ];

  ] /; 
    ( If[Length[equations] == Length[funcs],
        True,
        Message[RecursionOperator::eqns];False] && 
      If[Length[vars] == 2,
        True,
        Message[RecursionOperator::vars];False] && 
      If[
        (And @@ Map[
          (Count[#,q_/; MatchQ[q,Derivative[0,1][_][__]]] === 1 &&
           Count[#,q_/; ! FreeQ[q,Derivative[_,k_/;k>0][_][__]]] === 1)&,
           equations  /. Equal[a_,b_] :> a-b]),
        True,
        Message[RecursionOperator::dt, vars[[2]] ];False] &&
      If[
        FreeQ[ 
          equations,
          Power[a_,b_] /; ( ( Negative[b] || Head[b] === Symbol) && 
            Or @@ (! FreeQ[a, Head[#]]& /@ funcs) )
        ],
        True,
        Message[RecursionOperator::rational];False]
    );

End[] (* Ends `Private` *)

SetAttributes[RecursionOperator, ReadProtected];
SetAttributes[DefiningEquation, ReadProtected];

EndPackage[]

Print["The Recursion Operator package loaded successfully."];