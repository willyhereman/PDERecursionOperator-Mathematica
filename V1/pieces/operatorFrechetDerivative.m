(* : Title : FrechetDerivativeOfOperator *)
(* : Author : Douglas Baldwin *)
(* : Date : Sunday, July 07, 2002 *)

DefiningEquation`Operator`System`FrechetDerivative[system_, operator_] :=
  Module[{ frechetDebug = False,
           frechetDerivative,
           frechetDerivativeOfSystem
         },

    If[frechetDebug,
      Print["--------------------------------"];
      Print["system, The system inputed to FrechetDerivativeOfOperator:"];
      Print[CleanUpFunction[system]];
      Print["operator, The operator inputed to FrechetDerivativeOfOperator:"];
      Print[CleanUpFunction[operator]];
    ];

    (* Maps the Frechet derivative for operators over system  *)
    frechetDerivativeOfSystem = 
      Sum[
        DefiningEquation`Operator`Expression`FrechetDerivative[ 
          #, 
          system[[i]],
          RecursionOperator`UserFunctions[[i]] 
        ],
        {i, Min[Length[RecursionOperator`UserFunctions], Length[system] ] }
      ]& /@ #& /@ operator;

    If[frechetDebug,
      Print["frechetDerivativeOfSystem, is:"];
      Print[CleanUpFunction[frechetDerivativeOfSystem]];
      DefiningEquation`TeXPrint[frechetDerivativeOfSystem];
    ];

    Return[frechetDerivativeOfSystem];

  ];


(*------------------------------------------------------------------------------
-----------------*)

(* : Title : Operator Frechet Derivative *)
(* : Author : Douglas Baldwin *)
(* : Date : Wednesday, July 10, 2002 *)
(* : Summary : For each expression, the operator frechet derivative is *)
(*   computed.  $\sum_{i=0}^N \frac{\partial F}{\partial u_i}$ *)

DefiningEquation`Operator`Expression`FrechetDerivative[operator_, system_, 
function_] :=
  Module[{ frechetOperatorDebug = False,
           ii = -1,
           DefiningEquation`Operator`D,
           frechetDerivative
        },

    If[frechetOperatorDebug,
      Print["--------------------------------"];
      Print["The input to OperatorFrechetDerivative function is :"];
      Print["operator:  ", CleanUpFunction[operator] ];
      Print["system:  ", CleanUpFunction[system] ];
      Print["function:  ", CleanUpFunction[function] ];
    ];

    (* This first bit determines the maximum derivative in *)
    (* the expression for the given function. *)
    operator /. 
      { function :> 
          ( If[ii < 0, ii = 0];
            function
          ),
        Derivative[a__][f_][b__] :>
          If[ii < First[{a}], ii = First[{a}] ]
            /; f === Head[function]
      };

    If[frechetOperatorDebug,
      Print["The i of ", CleanUpFunction[operator], " for ", 
        CleanUpFunction[function], " is: ", CleanUpFunction[ii] ];
    ];


    (* Defines D for operators. *)
    DefiningEquation`Operator`D[terms__] :=
      DefiningEquation`Operator`Derivative[
        {terms},
        system,
        D[function, 
           {First[RecursionOperator`UserVariables ], i}
        ]
      ];

    frechetDerivative = 
      Sum[
        operator /. 
          DefiningEquation`Operator`Times -> 
            DefiningEquation`Operator`D,
        {i, 0, ii}
      ];

    If[frechetOperatorDebug,
      Print["frechetDerivative, is: "];
      Print[CleanUpFunction[frechetDerivative]];
    ];

    (* Clean up function *)
    frechetDerivative = 
      DefiningEquation`Operator`Expand[frechetDerivative];

    If[frechetOperatorDebug,
      Print["frechetDerivative, after simplifictation is: "];
      Print[CleanUpFunction[frechetDerivative]];
    ];

    Return[frechetDerivative];

  ];
