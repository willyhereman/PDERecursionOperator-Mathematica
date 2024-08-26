(* : Title : BuildCandidate *)
(* : Summary : 
  Builds the candidate recursion operator and outputs it in 
  the form needed for DefiningEquation. *)
(* : Author : Douglas Baldwin *)

BuildCandidate[equations_, funcs_, vars_, param_:{}, options___? OptionQ] :=
  Module[{ buildCandidateDebug = True, (* Debug bool. *)
           weights0, weights, (* The weights assoc. with the dilation sym. *)
           symmetries, (* The first seed + 1 symmetries. *)
           paramRestrictions = {}, (* Restrictions on the parameters. *)
           rank, (* The rank of the recursion operator. *)
           candidate0, (* The R0 component. *)
           candidate1, (* The R1 componenet. *)
           maxCoef (* The unknown coefficients. *)
         }, (* Protected Local Variables *)
    
    (* The symbol used for the unknown coefficients. *)
    coef = UnknownCoefficients /. {options} /. Options[RecursionOperator];

    (* Calculate the weights. *)
    weights0 = 
      Integrability`InvariantsSymmetries`Private`findWeights[
        "PDE", # == 0& /@ equations, Head /@ funcs, vars, options
      ];

    If[buildCandidateDebug,
      Print["The weights are:"];
      Print[CleanUpFunction[ weights0 ]];
    ];

    If[ 
      ! FreeQ[ 
        Flatten[ weights0 /. Rule[_,a_] :> a ], 
        Integrability`InvariantsSymmetries`Weight 
      ],
      Message[BuildCandidate::FreeWeight, weights0];
      Abort[];
    ];

    If[ Last[weights0] != {},
      Message[BuildCandidate::UnuniformWeights];
      Abort[];
    ];

    (* Puts weights in a more usable form. *)
    (* Specifically: Weight[u] -> 2 goes to {u[x,t], 2} *)
    weights = 
      Integrability`InvariantsSymmetries`Private`filterWeights[
        Head /@ funcs, vars, weights0, options];

    If[buildCandidateDebug,
      Print["filterWeights:"];
      Print[CleanUpFunction[ weights ]];
    ];

    If[weights === Null,
      Message[BuildCandidate::WeightsFail];
      Abort[];
    ];
    
    (* Generate Seed + 1 symmetries. *)
    symmetries = 
      # /. {a_List,b_List} :> 
        (paramRestrictions = Append[paramRestrictions, a];b)& /@
        #& /@
      BuildCandidate`getSymmetries[
        # == 0& /@ equations, Head /@ funcs, vars, 
        Seed /. {options} /. Options[RecursionOperator], 
        options,
        WeightedParameters -> param
      ];

    If[buildCandidateDebug,
      Print["The symmetries are:"];
      Print[CleanUpFunction[ symmetries ]];
    ];

    (* Compute the rank of the candidate recursion operator. *)
    rank = 
      Table[Last[#][[i]] - First[#][[j]], 
        {i, Length[equations]}, {j, Length[equations]}
      ]&[ First /@ symmetries ];

    If[buildCandidateDebug,
      Print["The rank of the recursion operator is:"];
      Print[CleanUpFunction[ rank ]];
    ];

    (* Generate the R0 components. *)
    {maxCoef, candidate0} = 
      BuildCandidate`BuildR0[
        weights,
        rank
      ];

    If[buildCandidateDebug,
      Print["Generate the \!\(R\_0\) component: "];
      Print[CleanUpFunction[ candidate0 ]];
    ];
    
    candidate1 = 
      BuildCandidate`BuildR1[
        equations,
        funcs,
        vars,
        rank,
        weights0,
        maxCoef,
        options
      ];

    If[buildCandidateDebug,
      Print["Generate the \!\(R\_1\) component: "];
      Print[CleanUpFunction[ candidate1 ]];
    ];

    If[Union[Flatten[candidate1]] === {0},
      If[
        InputString["When R1 is zero, it usually implies "<>
        "that the seed is wrong or the system doesn't have a recursion " <>
        "operator.  If you would like to continue, type \"Yes\".  If you " <>
        "would like to change the Seed option, type any other character."] !=
        "Yes",
        Abort[];
      ];
    ];

    Return[ candidate0 + candidate1 ];

  ];

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

BuildCandidate`BuildR0[weights_List, rank_List, options___? OptionQ] :=
  Module[{ buildR0Debug = True,
           coef, (* What form the coefficents are. *)
           ii = 1,
           myF,
           newWeights = First[weights],
           terms 
         }, (* Protected Local Variables *)

    coef = UnknownCoefficients /. {options} /. Options[RecursionOperator];

    myF = {First[#1] * First[#2], Last[#1] + Last[#2]}&;

    (* Augment weights. *)
    If[Length[newWeights] > 1,
      newWeights = 
        Table[
          Table[
            Table[
              myF[newWeights[[i]], 
                {Drop[newWeights, {i}][[j,1]]^k, k*Drop[newWeights, {i}][[j,2]]}
              ],
              {k, 1, 
                Floor[ 
                  ( Max[Flatten[rank]] - newWeights[[i,2]])/ 
                  Drop[newWeights, {i}][[j,2]] 
                ]
              }
            ],
            {j, Length[newWeights] - 1}
          ],
          {i, Length[newWeights]}        
        ];

      newWeights = Partition[Flatten[newWeights], 2];

      newWeights = Union[newWeights, First[weights]];

      If[buildR0Debug,
        Print["The new weights are:"];
        Print[CleanUpFunction[ newWeights ]];
      ];
    ];

    terms = 
      Table[
        Plus @@ (
          Sum[
            DefiningEquation`Operator`Times[ 
              operatorD[rank[[i,j]] - k*#[[2]]], 
              #[[1]]^k
            ],
            {k, 0, Floor[ rank[[i,j]]/#[[2]] ] }
          ]& /@ newWeights
        ),
        {i, Length[rank]},
        {j, Length[rank[[i]] ]}
      ] /. {} :> Sequence[];

    If[buildR0Debug,
      Print["The \!\(R\_0\) operator is:"];
      Print[CleanUpFunction[ terms ]];
    ];

    (* Simplifies the expression. *)
    terms = 
      DefiningEquation`Operator`Expand[terms];

    (* Removes the constants. *)
    terms = 
      terms //. a_[i_? NumericQ,b__] :> a[b] 
        /; a === DefiningEquation`Operator`Times;

    (* Append coefficents. *)
    terms = 
      terms /. DefiningEquation`Operator`Times[a__] :>
        DefiningEquation`Operator`Times[coef[ii++],a];
      
    If[buildR0Debug,
      Print["The \!\(R\_0\) operator with \!\(C\_i\) attached:"];
      Print[CleanUpFunction[ terms ]];
    ];

    Return[
        {ii, terms}
    ];

  ];

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

BuildCandidate`BuildR1[equations_List, funcs_List,
    vars_List, rank_List, weights_List, maxCoef_Integer,
    options___? OptionQ
  ] :=
  Module[{ buildR1Debug = True,
           weightUV, 
           symmetries,
           invariants,
           cosymmetries,
           R1, ii = maxCoef
         }, (* Protected Local Variables *)

    coef = UnknownCoefficients /. {options} /. Options[RecursionOperator];

    (* Weights for u,v,... *)
    weightUV = 
      ( Integrability`InvariantsSymmetries`Weight[
          #
        ]& /@ (Head /@ funcs) 
      ) /. Flatten[weights[[1]] ];


    symmetries = 
      Internal`DeactivateMessages[
        Integrability`InvariantsSymmetries`PDESymmetries[
          # == 0& /@ equations, Head /@ funcs, 
          vars, {0, rank[[1,1]] + weightUV[[1]] + 1},
          options
        ] //. {{True}, a_List} :> a,
        Integrability`InvariantsSymmetries`Weight::dilation,
        Integrability`InvariantsSymmetries`Weight::nonuniform1,
        Integrability`InvariantsSymmetries`Weight::nonuniform2,
        Solve::svars
      ] /. C[_] -> 1;

    If[buildR1Debug,
      Print["The symmetries are: "];
      Print[CleanUpFunction[ symmetries ]];
    ];
    
    invariants = 
      Internal`DeactivateMessages[
        Integrability`InvariantsSymmetries`PDEInvariants[
          # == 0& /@ equations, Head /@ funcs, 
          vars, {0, rank[[1,1]] + weightUV[[1]] + 1},
          options
        ] //. {{True}, a_List} :> a,
        Integrability`InvariantsSymmetries`Weight::dilation,
        Integrability`InvariantsSymmetries`Weight::nonuniform1,
        Integrability`InvariantsSymmetries`Weight::nonuniform2,
        Solve::svars
      ] /. C[_] -> 1;
      
    If[buildR1Debug,
      Print["The invariants are:"];
      Print[CleanUpFunction[ invariants ]];
    ];

    (* Computes the cosymmetries from the densities by taking the *)
    (* Euler operator of each density. *)
    cosymmetries = 
      { (Sequence @@ #[[1]]) - weightUV,
        Flatten[
          Integrability`InvariantsSymmetries`Private`EulerD[
            #,
            funcs,
            vars
          ]& /@ #
        ]& /@ #[[2]]
      }& /@ invariants;

    If[buildR1Debug,
      Print["The cosymmetries (Euler derivative of invariants) are:"];
      Print[CleanUpFunction[ cosymmetries ]];
    ];

    (* Composes the R1 operator from the symmetries and cosymmetries. *)
    R1 = 
      Table[
        Outer[
          DefiningEquation`Operator`Times,
          Transpose[
            symmetries[[i,2]] 
              /. Times -> DefiningEquation`Operator`Times
          ],
          {{operatorD[-1]}},
          cosymmetries[[Length[cosymmetries] - i + 1, 2]]
            /. Times -> DefiningEquation`Operator`Times
        ],
        {i, Length[symmetries]}
      ];

    (* Put it in the correct form and adds coef. *)
    R1 = 
      ( Plus @@ Union[Flatten /@ #& /@ R1] ) /. 
        DefiningEquation`Operator`Times[a__] :>
          DefiningEquation`Operator`Times[coef[ii++],a];

    If[buildR1Debug,
      Print["The \!\(R\_1\) operator is:"];
      Print[CleanUpFunction[ R1 ]];
    ];

    Return[R1];

  ];

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

BuildCandidate`getSymmetries[equations_List, funcs_List, vars_List, 
    n_Integer, options___] :=
  Module[{ i = 0,
           symmetries = {}
         }, (* Protected Local Variables *)

    While[ Length[symmetries] < n+1,
      i++;
      symmetries = 
        Join[
          symmetries,
          Internal`DeactivateMessages[
            Integrability`InvariantsSymmetries`PDESymmetries[
              equations, funcs, vars, i, 
              options
            ],
            Integrability`InvariantsSymmetries`Weight::dilation,
            Integrability`InvariantsSymmetries`Weight::nonuniform1,
            Integrability`InvariantsSymmetries`Weight::nonuniform2,
            Solve::svars
          ]
        ] /. {{(_Integer) ..}, {{(0)..}}} :> Sequence[];
    ];

    Return[symmetries];
  ];