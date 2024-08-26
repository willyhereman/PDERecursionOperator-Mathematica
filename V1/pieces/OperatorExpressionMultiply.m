(*------------------------------------------------------------------------------
-----------------*)
(* : Title : Dealing with the distributive properties of operators. *)
(* : Author : Douglas Baldwin *)
(* : Date : Thursday, June 27, 2002 *)

DefiningEquation`Operator`ExpressionMultiply[terms__] := 
  Block[{ operatorMultiplyDebug = False,
          result
         },

    If[operatorMultiplyDebug,
      Print["--------------------------------"];
      Print["The input to the OperatorMultiply function is :"];
      Print["terms:  ", CleanUpFunction[{terms}] ];
    ];

    (* The first thing to do is make lists out of the sums. *)
    (* This will allow us to use the outer product. *)
    result = 
      If[Head[#] === Plus,
        List @@ #,
        {#}
      ]& /@ {terms};

    If[operatorMultiplyDebug,
      Print["result, After a smart Plus -> List replacement: "];
      Print[CleanUpFunction[result]];
    ];
    

    (* We can compose the terms using the outer product. *)
    (* This is necessary because Mathematica doesn't expand *)
    (* a**(b+c) to a**b + a**c. *)
    result = 
      Outer[
        DefiningEquation`Operator`Times,
        Sequence @@ result
      ] /. List -> Plus;

    If[operatorMultiplyDebug,
      Print["result, After applying the outer product: "];
      Print[CleanUpFunction[result]];
    ];

    Return[result];

  ];
