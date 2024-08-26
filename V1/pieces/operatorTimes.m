(* : Title : Operator Times *)
(* : Author : Douglas Baldwin *)
(* : Date : Wednesday, March 10, 2004 *)

(* For testing purposes, I clear operatorTimes. *)
ClearAll[DefiningEquation`Operator`Times];

(* Sets up zero identity. *)
DefiningEquation`Operator`Times[___, 0, ___] := 0;

(* Sets up one identity. *)
DefiningEquation`Operator`Times[a___, 1, b___] := 
    DefiningEquation`Operator`Times[a, b];

(* Defines the distributive property for operator times. *)
DefiningEquation`Operator`Times[a___] :=
  ( ( DefiningEquation`Operator`Times @@ ({a} /. Plus -> List)
    ) /. List -> Plus
  ) /; (! FreeQ[{a}, Plus]);

(* Defines the multiplication of derivative operators. *)
DefiningEquation`Operator`Times[a___, operatorD[n_], operatorD[m_], b___] := 
    DefiningEquation`Operator`Times[a, operatorD[n + m], b] /; (n =!= -1);

DefiningEquation`Operator`Times[a___, operatorD[n_], operatorD[m_]] := 
    DefiningEquation`Operator`Times[a, operatorD[n + m] ];

(* Allows the identity to shift to the end of the expression. *)
DefiningEquation`Operator`Times[a___, operatorD[0], b__] := 
    DefiningEquation`Operator`Times[a, b, operatorD[0]];

(* Defines the actions of the operator derivative. *)
DefiningEquation`Operator`Times[a___, operatorD[n_?Positive], b__] := 
  Sum[
    DefiningEquation`Operator`Times[
      Binomial[n, k], 
      a, 
      ( D[Times @@ { b }, 
          {First[RecursionOperator`UserVariables], k}
        ] /. Times -> DefiningEquation`Operator`Times
      ), 
      operatorD[n - k]
    ], 
    {k, 0, n}
  ] /; FreeQ[{b}, operatorD ];

DefiningEquation`Operator`Times[a___, operatorD[n_?Positive], b__,
  operatorD[m_], c___ ] := 
  Sum[
    DefiningEquation`Operator`Times[
      Binomial[n, k], 
      a, 
      ( D[Times @@ { b }, 
          {First[RecursionOperator`UserVariables], k}
        ] /. Times -> DefiningEquation`Operator`Times
      ), 
      operatorD[n - k + m],
      c,
      operatorD[0]
    ], 
    {k, 0, n}
  ] /; FreeQ[{b}, operatorD ];

(* DB:3/9/2004 Removed in favor for above version. 
DefiningEquation`Operator`Times[a___, operatorD[n_?Positive], b_, c___] := 
  Module[{dDebug = False
         },

    If[dDebug,
      Print["------operatorD[i]------"];
      Print[
        CleanUpFunction[
          DefiningEquation`Operator`Times`Temp[a, operatorD[n], b, c]
        ], "\[DoubleLongRightArrow]"
      ];
      Print[
        CleanUpFunction[
          DefiningEquation`Operator`Times`Temp[a, 
            operatorD[n - 1], 
            ( D[b, First[RecursionOperator`UserVariables ] ] /. 
              Times -> DefiningEquation`Operator`Times
            ), 
            c
          ]
        ]
      ];
      Print[
        CleanUpFunction[
          DefiningEquation`Operator`Times`Temp[a, 
            operatorD[n - 1], 
            b, 
            operatorD[1], 
            c
          ]
        ]
      ];
    ];

    Return[
      Plus[
        DefiningEquation`Operator`Times[a, 
          operatorD[n - 1], 
          ( D[b, First[RecursionOperator`UserVariables ] ] /. 
              Times -> DefiningEquation`Operator`Times
          ), 
          c
        ],
        DefiningEquation`Operator`Times[a, 
          operatorD[n - 1], 
          b, 
          operatorD[1], 
          c
        ]
      ] /. Times -> DefiningEquation`Operator`Times
    ]
  ];
*)

(* Defines the actions of the operator integral. *)
DefiningEquation`Operator`Times[a___, operatorD[-1], u__, operatorD[n_?Positive] 
] := 
  Plus[
    Sum[
      DefiningEquation`Operator`Times[
        (-1)^k, 
        a, 
        ( D[Times @@ { u }, 
            {First[RecursionOperator`UserVariables], k}
          ] /. Times -> DefiningEquation`Operator`Times
        ), 
        operatorD[n - k - 1]
      ], 
      {k, 0, n-1}
    ],
    DefiningEquation`Operator`Times[
      (-1)^n,
      a,
      operatorD[-1],
      ( D[Times @@ { u }, 
          {First[RecursionOperator`UserVariables], n}
        ] /. Times -> DefiningEquation`Operator`Times
      ),
      operatorD[0]
    ]
  ] /; FreeQ[{u}, operatorD ];

(* DB:3/10/2004 removed for new version above. 
DefiningEquation`Operator`Times[a___, operatorD[-1], u__, operatorD[n_?Positive] 
] := 
  Module[{ integrateDebug = False
         },
    
    If[integrateDebug,
      Print["------operatorD[-1]------"];
      Print[
        CleanUpFunction[
          DefiningEquation`Operator`Times`Temp[a, operatorD[-1], u, operatorD[n] 
]
        ], 
        "\[DoubleLongRightArrow]"
      ];
      Print[
        CleanUpFunction[
          DefiningEquation`Operator`Times`Temp[a,
            u,
            operatorD[n-1]
          ]
        ]
      ];
      Print[
        CleanUpFunction[
          DefiningEquation`Operator`Times`Temp[-1,
              a,
              operatorD[-1], 
              ( D[Times[u], First[RecursionOperator`UserVariables ] ] /. 
                  Times -> DefiningEquation`Operator`Times`Temp
              ),
              operatorD[n-1]
            ]
          ]
      ];
    ];

    Return[
      Plus[
        DefiningEquation`Operator`Times[a,
          u,
          operatorD[n-1]
        ],
        DefiningEquation`Operator`Times[-1,
            a,
            operatorD[-1], 
            ( D[Times[u], First[RecursionOperator`UserVariables ] ] /. 
                Times -> DefiningEquation`Operator`Times
            ),
            operatorD[n-1]
        ]
      ]
    ];

  ] /; FreeQ[{u}, operatorD];
*)

(* Defines a frechet derivative for an operator expression *)
DefiningEquation`Operator`Derivative[terms_List, expression_, function_] :=
  Module[{ derivativeDebug = False,
           g, h, n, dF, 
           frechetDerivative 
         },

    (* Sets the values of g, h, and n *)
    terms /. {a___, operatorD[b_], c___} :> (g = {a}; n = b; h = {c});

    (* If Null, sets to zero. *)
    {g, h} = {g, h} /. {} :> {1}; 

    If[derivativeDebug,
      Print["------DefiningEquation`Operator`Derivative------\[NewLine]",
        "terms = ", CleanUpFunction[terms],
        "; expression = ", CleanUpFunction[expression],
        "; function = ", CleanUpFunction[function],
        "; \[NewLine]g = ", CleanUpFunction[g], 
        "; n = ", CleanUpFunction[n], 
        "; h = ", CleanUpFunction[h]
      ];
    ];

    (* Expands and computes D^i F *)
    dF = 
      DefiningEquation`Operator`Expand[
        DefiningEquation`Operator`Times[
          operatorD[i], 
          expression
        ]
      ] /. { Times -> DefiningEquation`Operator`Times,
             DefiningEquation`Operator`Times[___, operatorD[n_] ] :> 0 /; n =!= 
0
           };

    If[derivativeDebug,
      Print[
        StringReplace[
          "\!\( D\_x\^i F\) = ",
          "i" -> ToString[i]
        ], 
        CleanUpFunction[
          NonCommutativeMultiply[
            operatorD[i], 
            expression
          ]
        ],
        " = ",
        CleanUpFunction[ dF ] 
      ]
    ];

    If[ h === {1},
      frechetDerivative = 
        DefiningEquation`Operator`Times[
          dF,
          D[ (Times @@ g), function ], 
          operatorD[n]
        ],
      frechetDerivative = 
        Plus[
          DefiningEquation`Operator`Times[
            dF,
            D[ (Times @@ g), function ],
            operatorD[n],
            Sequence @@ h
          ],
          DefiningEquation`Operator`Times[
            Sequence @@ g,
            operatorD[n],
            dF,
            D[ (Times @@ h), function ]
          ]
        ] /. Times -> DefiningEquation`Operator`Times
    ];

    frechetDerivative = 
      DefiningEquation`Operator`Expand[ 
        frechetDerivative
      ];

    If[derivativeDebug,
      Print["frechetDerivative = ",
        CleanUpFunction[frechetDerivative],
        " for expression, ",
        CleanUpFunction[expression ],
        ", and function, ",
        CleanUpFunction[function ],
        "."
      ];
    ];

    Return[frechetDerivative ];

  ];

(* Sets the attributes of the operator times funciton *)
SetAttributes[DefiningEquation`Operator`Times, {Flat, Listable}];

(* Sets attribtues for DefiningEquation`Operator`Times`Temp *)
(* for testing purposes. *)
SetAttributes[DefiningEquation`Operator`Times`Temp, {Flat, Listable}];