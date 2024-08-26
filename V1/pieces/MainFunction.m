(* : Title : Defining Equation *)
(* : Author : Douglas Baldwin *)
(* : Date : Sunday, January 04, 2004 *)

DefiningEquation[system_, operator_, funcs_, vars_, param_:{} ] := 
  DefiningEquation[
    If[ Head[#] === List, #, {#}]&[system], 
    If[ Head[#] === List, #, {{#}}]&[operator], 
    If[ Head[#] === List, #, {#}]&[funcs], 
    If[ Head[#] === List, #, {#}]&[vars],
    If[ Head[#] === List, #, {#}]&[param]
  ];

DefiningEquation[system_List, operator_, funcs_List, vars_List, param_List ] :=
  Module[{primaryDebug = True,
          newOperator,
          frechetDerivativeOfSystem,
          operatorComposedWithFrechetDerivativeOfSystem,
          frechetDerivativeOfSystemComposedWithOperator,
          partialOperatorOverPartialT,
          frechetDerivativeOfOperator,
          definingEquationResult
         },

    (* Sets two global variables for funcs and vars. *)
    RecursionOperator`UserFunctions = funcs;
    RecursionOperator`UserVariables = vars;
    
    (* The following prints the input given by the user. *)
    If[primaryDebug,
      Print["system, The system entered by the user is:"];
      Print[CleanUpFunction[system]];
      Print["operator, The operator entered by the user is:"];
      Print[CleanUpFunction[operator]];
      Print["funcs, The dependent variables are:"];
      Print[CleanUpFunction[ funcs ]];
      Print["vars, The independent variables are:"];
      Print[CleanUpFunction[ vars ]];
      Print["param, The parameters are:"];
      Print[CleanUpFunction[ param ]];
    ];


    (* Converting the form of the operator for internal *)
    (* calculations. *)
    (* DB:1/4/2004: Added If. *)
    newOperator = 
      DefiningEquation`Operator`Expand[
        If[ FreeQ[operator, operatorD],
          DefiningEquation`ToOperator /@ #& /@ operator,
          operator
        ]
      ];

    If[primaryDebug,
      Print["newOperator, \[ScriptCapitalR]:  "];
      Print[CleanUpFunction[newOperator]]; 
      DefiningEquation`TeXPrint[newOperator];
    ];

    partialOperatorOverPartialT = 
      (
        newOperator /. 
          { DefiningEquation`Operator`Times[a___, t, b___] :> 
              DefiningEquation`Operator`Times`Temp[a,b],
            DefiningEquation`Operator`Times[a___, Power[t, n_], b___] :> 
              DefiningEquation`Operator`Times`Temp[n, a, Power[t, n-1], b],
            DefiningEquation`Operator`Times[a__] :> 0 /;
              FreeQ[{a}, t, 1]
          }
      ) /. DefiningEquation`Operator`Times`Temp -> 
DefiningEquation`Operator`Times;

    If[primaryDebug,
      Print["partialOperatorOverPartialT, " <>
        "\!\(\( \[PartialD]\[ScriptCapitalR] \) \/ \( \[PartialD] t \)\)"
      ];
      Print[CleanUpFunction[partialOperatorOverPartialT]];
    ];

    (* We then calculate the Frechet Derivative of the system. *)
    frechetDerivativeOfSystem = 
      DefiningEquation`Operator`Expand[
        DefiningEquation`System`FrechetDerivative[system]
      ];
    
    If[primaryDebug,
      Print["frechetDerivativeOfSystem, F'(u):  "];
      Print[CleanUpFunction[frechetDerivativeOfSystem]];
      DefiningEquation`TeXPrint[frechetDerivativeOfSystem];
    ];

    (* We then Compute $R \cirdot F'(u)$ *)
    operatorComposedWithFrechetDerivativeOfSystem = 
      DefiningEquation`Operator`Expand[
        Inner[
          DefiningEquation`Operator`ExpressionMultiply,
          newOperator,
          frechetDerivativeOfSystem,
          Plus
        ]
      ];

    If[primaryDebug,
      Print["operatorComposedWithFrechetDerivativeOfSystem, 
\[ScriptCapitalR]\[SmallCircle]F'(u):  "];
      Print[CleanUpFunction[operatorComposedWithFrechetDerivativeOfSystem]];
      DefiningEquation`TeXPrint[operatorComposedWithFrechetDerivativeOfSystem];
    ];
   
    (* We then Compute $F'(u) \cirdot R$ *)
    frechetDerivativeOfSystemComposedWithOperator = 
      DefiningEquation`Operator`Expand[
        Inner[
          DefiningEquation`Operator`ExpressionMultiply,
          frechetDerivativeOfSystem,
          newOperator,
          Plus
        ]
      ];

    If[primaryDebug,
      Print["frechetDerivativeOfSystemComposedWithOperator, 
F'(u)\[SmallCircle]\[ScriptCapitalR]:  "];
      Print[CleanUpFunction[frechetDerivativeOfSystemComposedWithOperator]]; 
      DefiningEquation`TeXPrint[frechetDerivativeOfSystemComposedWithOperator];
    ];

    frechetDerivativeOfOperator = 
      DefiningEquation`Operator`Expand[
        DefiningEquation`Operator`System`FrechetDerivative[
          system,
          newOperator
        ]
      ];

    If[primaryDebug,
      Print["frechetDerivativeOfOperator, \[ScriptCapitalR]'[F]:  "];
      Print[CleanUpFunction[frechetDerivativeOfOperator]];
      DefiningEquation`TeXPrint[frechetDerivativeOfOperator];
    ];

    definingEquationResult = 
        DefiningEquation`Operator`Constraints[
          partialOperatorOverPartialT 
          + frechetDerivativeOfOperator 
          + operatorComposedWithFrechetDerivativeOfSystem
          - frechetDerivativeOfSystemComposedWithOperator,
          param
        ];

    If[ definingEquationResult =!= {True, {}},
      DefiningEquation`TeXPrint[
        (newOperator /. #)& /@ definingEquationResult,
        TeXPrint->True
      ]
    ];

    Return[
      If[ definingEquationResult === {True, {}},
        {True, {}},
        {False, 
          {newOperator /. #, #}& /@ definingEquationResult }
      ]
    ];
   
  ];