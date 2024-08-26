(* : Title : CleanUpFunction *)
(* : Author : Douglas Baldwin *)
(* : Date : Sunday, June 23, 2002 *)
(* : Description : A little function to clean up the output before *)
(*   it's sent to the user. *)

CleanUpFunction = 
  (
    ToExpression[ 
      StringReplace[
        ToString[
          InputForm[#]
        ],
        "recursionOperator`Private`" -> ""
      ]
    ] //. 
    { Derivative[n__][u_[a_,b_]][x__] :>
         SequenceForm[
           Subscript[u,
             SequenceForm["(",a,",",b,")"]
           ], 
           Subscript[
             SequenceForm @@ 
               Flatten[
                 Table[#[[1]], 
                   {#[[2]]}
                 ]& /@
                   Transpose[{{x}, {n}}]
               ]
           ]
         ],
      Derivative[n__][F[a_]][x__] :>
        SequenceForm[
          Subscript[u,a],
          Subscript[
            SequenceForm @@ 
              Flatten[
                Table[#[[1]], {#[[2]]}]& /@
                  Transpose[{{x}, {n}}]
              ]
          ]
        ],
      Derivative[n__][F_[a_]][x__] :>
        SequenceForm[
          Subscript[F,a],
          Subscript[
            SequenceForm @@ 
              Flatten[
                Table[#[[1]], {#[[2]]}]& /@
                  Transpose[{{x}, {n}}]
             ]
          ]
        ],
      Derivative[n__][g_][x__] :>
        SequenceForm[g,
          Subscript[
            SequenceForm @@ 
              Flatten[
                Table[#[[1]], {#[[2]]}]& /@
                  Transpose[{{x}, {n}}]
              ]
            ]
          ],
      F[a_][__] :> Subscript[u,a],
      u[a_,b_][__] :>
        Subscript[u,
          SequenceForm["(",a,",",b,")"]
        ],
      u[a_,b_] :> 
        Subscript[u,
          SequenceForm["(",a,",",b,")"]
        ]
      }
    ) &;

(* : Title : Pretty Print *)
(* : Author : Douglas Baldwin *)
(* : Date : Monday, July 01, 2002 *)
(* : Description : A little function to clean up the output before *)
(*   it's sent to the user. *)

PrettyPrint = 
  Print[
    CleanUpFunction[# /. 
      { DefiningEquation`Operator`Times -> Dot,
        operatorD[0] -> "I",
        operatorD[n_] :> 
          \!\(D\_(First[RecursionOperator`UserVariables])\^ToString[n]\)
      }
    ] /.
    { Derivative[n__][u_][x__] :>
        SequenceForm[u, 
          Subscript[
            SequenceForm @@ 
              Flatten[Table[#[[1]], {#[[2]]}] & /@ Transpose[{{x}, {n}}] ]
          ]
        ],
      u_[Sequence @@ CleanUpFunction[RecursionOperator`UserVariables]] :> u 
    }
  ]&;

DefiningEquation`TeXPrint[expression0_, opts__:{TeXPrint->False}] :=
  Module[{ expression = DefiningEquation`Operator`Expand[expression0],
           x = ToString[ First[ RecursionOperator`UserVariables ] ],
           operator`Times`Converter,
           nm = Dimensions[expression]
         },

    If[TeXPrint /. opts,

      operator`Times`Converter = 
        ( ( ToString /@ {##} ) /. List -> StringJoin )&;

      output = 
        InputForm[ expression ] /. 
          { operatorD[0] :> "I",
            operatorD[i_] :> "D_" <> x <> "^" <> ToString[{i}],
            Derivative[n__][u_][x__] :> 
              ToString[ u ] <> "_" <> 
                StringReplace[
                  ToString[
                    Flatten[
                      Transpose[{{n}, {x}}] /. { {0, _} :> Sequence[], {1, a_} 
:> a }
                    ]
                  ],
                  "," -> ""
                ],
            u_[Sequence @@ RecursionOperator`UserVariables] :> ToString[u]
          };

      output = 
        output /. { Rational[a_, b_] :> "\frac{" <> ToString[a] <> "}{" <> 
ToString[b] <> "}",
                    Power[a_, b_] :> 
                      If[Negative[b],
                        "\frac{1}{" <> ToString[a] <> "^{" <> ToString[-b] <> 
"}}",
                        ToString[a] <> "^{" <> ToString[b] <> "}"
                      ]
                  };

      output = output /. DefiningEquation`Operator`Times -> 
operator`Times`Converter;

      output = 
        StringReplace[
          ToString[ output ],
          { "recursionOperator`Private`" -> "",
            "\"" -> ""
          }
        ];

      Print[output];

    ];

  ];

