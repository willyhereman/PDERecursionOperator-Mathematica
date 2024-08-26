(* : Title : Operator Factor *)
(* : Author : Douglas Baldwin *)
(* : Date : Friday, July 19, 2002 *)

DefiningEquation`Operator`Constraints[system_, param_] :=
  Module[{ factorDebug = True,
           result
         },

    (* The first step is to expand the expression. *)
    result = 
      DefiningEquation`Operator`Expand[system];

    If[factorDebug,
      Print["--------Constraints--------"];
      Print["result, after expansion is:  "];
      Print[CleanUpFunction[ result ]];
    ];

    (* If everything simplifes, it returns True. *)
    If[ Union[ Flatten[ result ] ] === {0},
      Return[ {True, {} } ]
    ];

    theVariables = 
      Select[
        Variables[result /. { List -> Plus, 
                              DefiningEquation`Operator`Times -> Times}
        ],
        FreeQ[#, 
          Alternatives @@ 
            Join[RecursionOperator`UserFunctions, 
              RecursionOperator`UserVariables,
              {operatorD}
            ] 
        ]&
      ];
    theVariables = Complement[theVariables, param];
    
    If[factorDebug,
      Print["theVariables: "];
      Print[CleanUpFunction[ theVariables ]];
    ];
    
    (* Maps the Constraints function around the result. *)
    result = 
      MapAll[Factor, 
        Union[
          Flatten[
            DefiningEquation`Operator`Constraints /@ #& /@ result
          ]
        ]
      ];

    If[factorDebug,
      Print["result, the equations to be solved: "];
      Print[CleanUpFunction[ result ]];
    ];

    (* DB:1/5/2004 Added the removal of True. *)
    result = 
      ( Flatten /@ 
        analyzeSystem[result, theVariables, param]
      ) /. True :> Sequence[];

    If[factorDebug,
      Print["result, after solving for theVariables: "];
      Print[CleanUpFunction[ result ]];
    ];

(* Removed by DB:1/4/2004
    result = 
      Join[
        Thread[theVariables -> 
          ( Min[Complement[ Abs /@ #, {0}] ]^(-1) * # ) & 
            [theVariables /. # /. ( (#-> 1) & /@ theVariables) ] 
        ],
        Thread[ param -> (param /. #) ]
      ] & /@ result;
  
    result = 
      Join[
        Thread[ theVariables -> 
          ( theVariables /. # /. ( (#-> 1) & /@ theVariables) )
        ],
        Thread[ param -> (param /. #) ]
      ] & /@ result;

    If[factorDebug,
      Print["Setting missing terms to 1: "];
      Print[CleanUpFunction[ result ]];
    ];
*)

    Return[ result ];

  ] /; Head[system] === List;


DefiningEquation`Operator`Constraints[expression_] :=
  Module[{ factorDebug = True,
           result,
           operatorList = {},
           nonZeros
         },

    (* We then convert the expression a+b -> {a,b} *)
    result = expression /. Plus -> List;
    If[Head[result] =!= List, 
      result = {result}
    ];


    If[factorDebug,
      Print["result, after Plus -> List"];
      Print[CleanUpFunction[ result ]];
    ];

    (* Converts DefiningEquation`Operator`Times to a list. *)
    result = 
      Sort[
        result /. { DefiningEquation`Operator`Times[a___, operatorD[n_], 
                      b___] :>
                      {{operatorD[n], b}, Times[a] },
                    DefiningEquation`Operator`Times[a___] :>
                      {{}, Times[a] } /; FreeQ[{a}, operatorD]
                  }
      ];

    If[factorDebug,
      Print["Converts DefiningEquation`Operator`Times to a special list."];
      Print[CleanUpFunction[ result ]];
    ];

    result = 
      Split[result, #1[[1]] === #2[[1]]&];

    If[factorDebug,
      Print["Combine like lists: "];
      Print[CleanUpFunction[ result ]];
    ];

    result = 
      MapAll[Factor, Plus @@ (Last /@ #) & /@ result];

    If[factorDebug,
      Print["Collapses into correct form:"];
      Print[CleanUpFunction[ result ]];
    ];

(* Removed for alt. version from Unal
    result = 
      Union[
        Flatten[
          If[Head[#] === Plus, {Coefficient[#, RecursionOperator`UserFunctions, 
0], List @@ #, {#}] &
            [ Collect[#, {u[x, t], Derivative[_, 0][u][x, t]}] ] & /@ 
              result
        ] /. (# -> 1& /@ RecursionOperator`UserFunctions) 
      ];
*)

    result = 
      Union[
        Flatten[
          buildEquationList[#,
              RecursionOperator`UserFunctions,
              RecursionOperator`UserVariables
          ]& /@ result
        ]
      ];

    If[factorDebug,
      Print["Collecting like terms, we get the system:"];
      Print[CleanUpFunction[ result ]];
    ];

    Return[ result ];  
  
  ] /; Head[expression] =!= List;

(* : Title : Analyze and Build System *)
(* : Author : Unal Goktas and Willy Hereman *)
(*   Modified by Douglas Baldwin to work for this code. *)

analyzeSystem[system_List, unknown_List, parlist_List, opts___]:=
    Module[{i,inputlist,output = {}},
      If[parlist === {},
        Return[ 
          Internal`DeactivateMessages[
            Solve[system,unknown], 
            Solve::svars
            ]
          ]
        ];

      inputlist = unknown;

      While[inputlist =!= {},
         Module[{j,input,cond,tempsol,parsol},
             input = ToRules[First[inputlist] == 1];
             inputlist = Drop[inputlist,1];
             cond = Eliminate[system /. input,unknown];
             If[cond =!= False,
                If[cond === True,
                   tempsol = 
                     Union[
                       Flatten[
                         Internal`DeactivateMessages[
                           Solve[system /. input,unknown],
                           Solve::svars
                         ]],
                       input
                     ];
                   inputlist = Intersection[inputlist,
                       Union[Cases[tempsol,w_/;Last[w] == 0] /. (w_-> 0)->w,
                             Complement[inputlist,First[#]& /@ tempsol]]];
                   output = Append[output,{{cond}, tempsol}],
                   parsol = DeleteCases[
                               Internal`DeactivateMessages[
                                 Solve[Reduce[cond,parlist],parlist],
                                 Solve::svars
                               ],w_/;
                               Intersection[w,(# -> 0 &) /@ parlist] =!= {}];
                   Do[
                     tempsol = 
                      Union[
                        Flatten[
                          Internal`DeactivateMessages[
                            Solve[system /. parsol[[j]] /. input,
                              unknown
                            ], 
                            Solve::svars
                          ]
                        ],
                        input
                      ];
                     inputlist = Intersection[inputlist,
                       Union[Cases[tempsol,w_/;Last[w] == 0] /. (w_-> 0)->w,
                             Complement[inputlist,First[#]& /@ tempsol]]];
                     output = Append[output,{parsol[[j]], tempsol}],
                     {j,Length[parsol]}
                     ]
                  ]
               ]
             ]
         ];
      If[output =!= {},
        Return[output],
        Return[{Table[0,{Length[unknown]}]}]
      ]
      ]

myLength[expr_]:= If[Head[expr] === Plus,Length[expr],1]

(* Uses separator *)
buildEquationList[expr_,funcs_,vars_] :=
    Module[
      {nexpr = Expand[expr],eqlist = {},list1,list2,same,i,j},
      eqlist = {eqlist,nexpr /. u_[_,_] :> 0};
      nexpr = nexpr-(nexpr /. u_[_,_] :> 0);
      If[nexpr =!= 0,
        list1 = separator[nexpr,funcs,vars];
        list2 = Union[Table[Part[Part[list1,i],2],{i,Length[list1]}]];
        Do[
          same = Select[list1,SameQ[#[[2]],Part[list2,i]]&];
          eqlist = {eqlist,Sum[Part[Part[same,j],1],{j,Length[same]}]},
          {i,Length[list2]}
          ];
        ];
      (# == 0)& /@ DeleteCases[Union[Flatten[eqlist]],x_0]
          ];

separator[expr_,funcs_,vars_] :=
    Module[
      {expr1 = Expand[expr],length,expr2,expr3,coef,list = hH[],expr3j,i,j},
      If[Head[expr1] === Plus,length = Length[expr1],length = 1];
      Do[
        If[length == 1,expr2 = expr1,expr2 = Part[expr1,i]];
        coef = 1;
        expr3 = FactorList[expr2];
        Do[
          expr3j = Part[expr3,j];
          If[And @@ (FreeQ[expr3j,#]& /@ Union[funcs,vars]),
            coef = coef*Part[expr3j,1]^Part[expr3j,2];
            expr2 = expr2/Part[expr3j,1]^Part[expr3j,2];
            ],
          {j,Length[expr3]}];
        list = hH[list,{coef,expr2}],
        {i,length}];
      List @@ Flatten[list,Infinity,hH]
          ];