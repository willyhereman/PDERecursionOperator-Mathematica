(* : Title : Operator Expand *)
(* : Author : Douglas Baldwin *)
(* : Date : Thursday, July 11, 2002 *)

DefiningEquation`Operator`Expand[expression___] :=
  Module[{ simplifyDebug = False,
           temp,
           result
         },

    If[simplifyDebug,
      Print["------Expand------"];
      Print["expression, The input from the user"];
      Print[CleanUpFunction[ expression ]];
    ];
    
    If[Dimensions[expression] === {},
      result = 
        Flatten[{expression /. Plus -> List}] /. List -> Plus,
      result = 
        (Flatten[{# /. Plus -> List}] /. List -> Plus)& /@
          #& /@ expression
    ];


    If[simplifyDebug,
      Print["result, after distribution"];
      Print[CleanUpFunction[ result ]];
    ];

    result = 
      ReplaceAll[(result /. Times -> DefiningEquation`Operator`Times),
        DefiningEquation`Operator`Times[terms__] :>
          ( temp = 1;
            Replace[{terms}, 
              a_ :> (temp *= a) /; 
                FreeQ[a, 
                  Alternatives @@ Join[RecursionOperator`UserFunctions, 
                                    RecursionOperator`UserVariables,
                                    {operatorD}
                                  ] 
                ], 
              1
            ];
            temp * DefiningEquation`Operator`Times[
                     Sequence @@ Replace[{terms},  
                                   a_ :> Sequence[] /; 
                                     FreeQ[a, 
                                       Alternatives @@ 
Join[RecursionOperator`UserFunctions, 
                                         RecursionOperator`UserVariables,
                                         {operatorD}
                                       ] 
                                     ], 
                                   1
                     ]
                   ]
          )
      ];

    If[simplifyDebug,
      Print["result, after numeric carried left"];
      Print[CleanUpFunction[ result ]];
    ];
    
    
    result = 
      ReplaceAll[result,
       DefiningEquation`Operator`Times[terms__] :>
         DefiningEquation`Operator`Times[
           Sequence @@ 
             ReplaceRepeated[
               {terms},
               { {a___, operatorD[n_]} :>
                   { ( Sequence @@ If[Head[#] === Times, #, {#}]&[Times[a] ] ), 
                     operatorD[n]
                   } /; FreeQ[{a}, operatorD],
                 {a___, operatorD[n_], b__, operatorD[m_]} :>
                   { ( Sequence @@ If[Head[#] === Times, #, {#}]&[Times[a] ] ), 
                     operatorD[n], 
                     ( Sequence @@ If[Head[#] === Times, #, {#}]&[Times[b] ] ), 
                     operatorD[m]
                   } /; (FreeQ[{a}, operatorD] && FreeQ[{b}, operatorD])
               }
             ]
         ]
      ];

    If[simplifyDebug,
      Print["result, after term grouping"];
      Print[CleanUpFunction[ result /. Times -> 
DefiningEquation`Operator`Times]];
    ];

    Return[result /. Times -> DefiningEquation`Operator`Times];
  
  ];