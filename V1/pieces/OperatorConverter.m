(* : Title : OperatorConversion *)
(* : Author : Douglas Baldwin *)
(* : Date : Wednesday, July 10, 2002 *)

DefiningEquation`ToOperator[operator_] :=
  Block[{ operatorDebug = False,
           myD,
           integrate
         },

    If[operatorDebug,
      Print["--------------------------------"];
      Print["operator, The operator inputed to OperatorConverter"];
      Print[CleanUpFunction[operator]];
    ];

    (* Properties of myD *)
    myD[a__] := D[a] /; FreeQ[{a}, operatorD];

    myD[a_, b_] :=
      DefiningEquation`Operator`Times[
        operatorD[
          If[Head[b] === List,
            b[[-1]],
            1
          ]
        ],
        ( a /. 
          operatorD[Sequence @@ RecursionOperator`UserVariables] :> Sequence[] 
        )
      ] /; (! FreeQ[{a}, operatorD]);

    (* Properties of integrate *)
    integrate[a__] := Integrate[a] /;  FreeQ[{a}, operatorD];

    integrate[a__] :=
      DefiningEquation`Operator`Times[
        operatorD[-1],
        ( First[{a}] /. 
          operatorD[Sequence @@ RecursionOperator`UserVariables] -> Sequence[] 
        )
      ] /;  (! FreeQ[{a}, operatorD]);

    (* Performs the actual conversion *)
    newOperator = 
       Release[
         Hold[operator] /. 
           { Times -> DefiningEquation`Operator`Times, 
             Plus -> List,
             Integrate -> integrate, 
             D -> myD
           }
       ];

    If[operatorDebug,
      Print["newOperator, converts *,+,D and Integrate to internal notation:"];
      Print[CleanUpFunction[newOperator]]
    ];
    
    newOperator = 
      ( newOperator[ operatorD[Sequence @@ RecursionOperator`UserVariables] ] 
      ) /. operatorD[Sequence @@ RecursionOperator`UserVariables] :> 
operatorD[0];

    If[operatorDebug,
      Print["newOperator, substitutes operatorD[x,t] into operator:"];
      Print[CleanUpFunction[newOperator]]
    ];

    newOperator = 
      newOperator /. DefiningEquation`Operator`Times[a___, b_] :>
        DefiningEquation`Operator`Times[a,b,operatorD[0] ] /; Head[b] =!= 
operatorD;
      

    If[operatorDebug,
      Print["newOperator, checks that expressions ends with operatorD[i]:"];
      Print[CleanUpFunction[newOperator]]
    ];

    (* Simplifies Further *)
    newOperator = 
      (newOperator /. Plus -> List) /. List -> Plus;

    If[operatorDebug,
      Print["newOperator, distributes:"];
      Print[CleanUpFunction[newOperator]]
    ];

    Return[newOperator];
  ];