(* : Title : FrechetDerivativeOfSystem *)
(* : Author : Douglas Baldwin *)
(* : Date : Thursday, June 27, 2002 *)

DefiningEquation`System`FrechetDerivative[system_] :=
  Block[{frechetDebug = False,
          frechetDerivative,
          frechetDerivativeOfSystem
         },

    If[frechetDebug,
      Print["--------------------------------"];
      Print["system, The system inputed to FrechetDerivativeOfSystem:"];
      Print[CleanUpFunction[system]];
    ];

    (* Maps the Frechet derivative for operators over system  *)
    frechetDerivativeOfSystem = 
      Table[
        Table[
          DefiningEquation`Expression`FrechetDerivative[ 
            system[[j]], 
            RecursionOperator`UserFunctions[[i]] 
          ],
          {i, Length[RecursionOperator`UserFunctions]}
        ],
        {j, Length[system]}
      ];

    If[frechetDebug,
      Print["frechetDerivativeOfSystem, is:"];
      Print[CleanUpFunction[frechetDerivativeOfSystem//MatrixForm]];
    ];

    Return[frechetDerivativeOfSystem];

  ];


(*------------------------------------------------------------------------------
-----------------*)

(* : Title : Expression Frechet Derivative *)
(* : Author : Douglas Baldwin *)
(* : Date : Thursday, June 27, 2002 *)
(* : Summary : For each expression, the operator frechet derivative is *)
(*   computed.  $\sum_{i=0}^N \frac{\partial F}{\partial u_i}$ *)

DefiningEquation`Expression`FrechetDerivative[expression_, function_] :=
  Block[{ frechetExpressionDebug = False,
          ii = -1,
          frechetDerivative
        },

    If[frechetExpressionDebug,
      Print["--------------------------------"];
      Print["The input to ExpressionFrechetDerivative function is :"];
      Print["expression:  ", CleanUpFunction[expression] ];
    ];

    (* This first bit determines the maximum derivative in *)
    (* the expression for the given function. *)
    expression /. 
      { function :> 
          ( If[ii < 0, ii = 0];
            function
          ),
        Derivative[a__][f_][b__] :>
          If[ii < First[{a}], ii = First[{a}] ]
            /; f === Head[function]
      };

    If[frechetExpressionDebug,
      Print["The i of ", CleanUpFunction[expression], " for ", 
        CleanUpFunction[function], " is: ", CleanUpFunction[ii] ];
    ];

    (* We then apply the formula directly and return the result. *)
    frechetDerivative = 
      Sum[
        DefiningEquation`Operator`Times[
          ( D[expression, 
              D[function, 
                {First[RecursionOperator`UserVariables ], i}
              ] 
            ] /. 
            Times -> DefiningEquation`Operator`Times
          ),
          operatorD[i]
        ],
        {i, 0, ii}
      ];

    If[frechetExpressionDebug,
      Print["frechetDerivative, is: "];
      Print[CleanUpFunction[frechetDerivative]];
    ];

    Return[frechetDerivative];

  ];