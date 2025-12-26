 
(* Last updated: changes marked with WH Aug 24, 2024 *)
(* Previoulsy updated: WH Jul 4, 2024 and UG -- Jun 27, 2024 *)

(* New PrettyPrint designed by Unal is included. *)
(* Fix of the subscript notation by Unal is included. *)

(* Tried to fix the print form of output so that there is no confusion *)
(* about terms that follow D_x^{-1} or are coefficients of D_x^{-1} *)

(* Changes marked with WH Jun 24, 2024, WH Jun 25, 2024, WH Jun 27, 2024 *)
(* WH Jul 4, 2024, and WH Aug 24, 2024 *)

(* Hereman *)
(* : Title : Recursion Operator *)
(* : Author : Douglas Baldwin *)
(* : Date : Saturday, June 23, 2007 at 3:34 PM *)

Get["InvariantsSymmetries.m"];

(* WH Jul 4, 2024 *)
Print["Loading Recursion Operator Package of September 20, 2007."];
Print["Last updated on August 24, 2024 by Willy Hereman and Unal Goktas."];

(* Starts package environment. *)
BeginPackage["recursionOperator`","Integrability`InvariantsSymmetries`"]

(* Defines usage. *)
DefiningEquation::usage = 
"DefiningEquation[] brings up a menu of built in examples. \[NewLine]
DefiningEquation[eqn, opeartor, funcs, vars, opts] checks the 
recursion operator using its defining equation."

RecursionOperator::usage = 
"RecursionOperator[eqn, funcs, vars, param]."

BuildCandidate::FreeWeight = 
"There is freedom in the dilation symmetry `1` for this equation.  Please
set the free weight by using the option WeightRules.  For instance, if
Weight[u] isn't fixed, try WeightRules -> {Weight[u] -> 1}."

RecursionOperator::failed = 
"A valid recursion operator wasn't found.  Please try setting the Seed option
to an interger greater than one.  For example, Seed -> 2."

BuildCandidate::WeightsFail = 
"This system doesn't have a dilation symmetry, a requirement of this algorithm."

BuildCandidate::UnuniformWeights = 
"One of the terms in this system has terms of unequal rank.  The current version
of this package can't deal with this case."

RecursionOperator::eqns = 
"This package requires that the number of functions is equal to the number of
equations."

RecursionOperator::vars = 
"This package is only designed for two independent variable system, also
known as (1+1) dimensional systems."

RecursionOperator::dt = 
"This package is only designed to handle terms with at most one derivative
in `1`.  If you have a term like D[u[x,t], {t,2}], then rewrite it as
D[v[x,t], t] and add the equation D[u[x,t], t] == v[x,t]."

RecursionOperator::rational = 
"This package is only designed to work with polynomial systems with
fixed powers."

weight::nonuniform1 =
"Given system has at least one equation with terms of unequal rank,
so that scaling properties can not be determined."
  
weight::nonuniform2 =
"Incompatibility has been cured by assuming auxiliary parameters
with weight."

weight::nonuniform3 =
"The weights, `1`, could not be fixed. The weights may be fixed using the
WeightRules option."

weight::nonuniform4 =
"Incompatibility could not been cured. Using the options WeightedParameters
or WeightRules may help."

weight::needMoreInformation = 
"The weights cannot be resolved without additional information.  The weights
as calculated are `1`.  Please use the WeightRules option to provide additional
information, e.g. WeightRules -> {weight[u] -> 2} or
WeightRules -> {weight[u] -> weight[v]}."

(* WH Jun 24, 2024 Options[RecursionOperator] *)
(* 
Options[RecursionOperator] =
  { Seed -> 1, UnknownCoefficients -> C,
    WeightedParameters -> {},
    WeightRules -> {},
    MaxExplicitDependency -> 0,
    RankShift -> 0,
    (* WH Jun 24, 2024 *)
    (*  was Verbose -> False, *)
    Verbose -> True, 
    (* was WeightsVerbose -> True *)
    WeightsVerbose -> False
  }
*)

Options[RecursionOperator] =
  { Seed -> 1, UnknownCoefficients -> C,
    WeightedParameters -> {},
    WeightRules -> {},
    MaxExplicitDependency -> 0,
    RankShift -> 0,
    Verbose -> False,
    WeightsVerbose -> False
  }

(* Begins private environment. *)
Begin["`Private`"]

(* Added on 1/11/2003 to remove message Solve::svars when using *)
(* operatorConstraints with constants.  Code from Unal Goktas.  *)
If[$VersionNumber < 4,
   Attributes[Internal`DeactivateMessages] = {HoldAll};
   Internal`DeactivateMessages[body_, msgs___MessageName] :=
      Module[{wasOn = Select[Hold[msgs], Head[#] =!= $Off &], result},
         CheckAbort[
            Scan[Off, wasOn];
            result = body;
            Scan[On, wasOn];
            result,
            Scan[On, wasOn];
            Abort[]
         ]
      ]
 ]; (* end if *)
(* WH Aug 24, 2024 added ; in line above *)
(* Takes the input and puts it in the correct form *)
RecursionOperator[equations_, funcs_, vars_, param_List:{},
  options___? OptionQ] :=
  RecursionOperator[
    If[ Head[#] === List, #, {#}]&[equations], 
    If[ Head[#] === List, #, {#}]&[funcs], 
    If[ Head[#] === List, #, {#}]&[vars],
    If[ Head[#] === List, #, {#}]&[param],
    options
  ] /; ! And[ Head[equations] === List, Head[funcs] === List,
    Head[vars] === List, Head[param] === List];

(* Generates a candidate form and then tests that form. *)
RecursionOperator[equations_List, funcs_List, 
    vars_List, param_List:{}, options___? OptionQ] :=
  Module[{ recursionOperatorDebug = 
            (Verbose /. {options} /. Options[RecursionOperator]),
           candidate,
           solution
         }, (* Protected Local Variables *)
    (* Sets two global variables for funcs and vars. *)
    RecursionOperator`UserFunctions = funcs;
    RecursionOperator`UserVariables = vars;
    candidate = 
      BuildCandidate[equations /. Equal[a_,b_] :> a-b, 
        funcs, vars, param, options];
    (* WH Jun 24, 2024 *) 
    (* WH Aug 24, 2024 *) 
    If[recursionOperatorDebug,
      Print["The candidate recursion operator \[ScriptCapitalR] is: "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[ candidate ]]];
      Print["IMPORTANT: Always look at the PrettyPrint forms to see which pieces" <>
      " are under the operators (that is, on the right side of the operator), and" <>
      " which pieces are factors (on the left side of the operator)."]
    ];
    (* WH Jun 27, 2024 *)      
    (* WH Aug 24, 2024 *)
    If[recursionOperatorDebug,
      Print["The PrettyPrint form of the candidate recursion operator \[ScriptCapitalR] is: "];
      Print[PrettyPrint[ candidate ]];
      Print["Number of terms in candidate operator \[ScriptCapitalR] is: "];
      (* the line below worked for a scalar recursion operator *)
      (* Print[Length[ Part[candidate[[1]],1] ]] *)
      Print[ Map[Length, Flatten[ candidate ]] ];
      Print["For \[ScriptCapitalR] in matrix form, these are the number of terms in" <>
      " the elements of the matrix (going from left to right, row by row)."] 
    ]; 
    solution = 
      First /@ 
      DefiningEquation[equations /. Equal[a_,b_] :> b,
        candidate, funcs, vars, param, options
      ][[2]];
    (* WH Aug 24, 2024 *)
    If[recursionOperatorDebug,
      Print["The candidate after being run through the defining equation: "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[ solution ]]];
      Print["PrettyPrint form of the operator: "];
      Print[PrettyPrint[ solution ]];
      Print["The computed recursion operator will be tested!"];
      Print["The code will test if the operator indeed satisfies" <>
            " the defining equation."];
     ];
    If[ Union[Flatten[solution]] === {0},
      Message[RecursionOperator::failed];
      Abort[];
    ];      
    If[
      Or @@ 
      ( First /@
        ( DefiningEquation[equations /. Equal[a_,b_] :> b,
            #, funcs, vars, param, options
          ]& /@ solution ) ), 
       (* end of then part of if statement, start else part of if statement *)
       (* WH Jun 24, 2024 *)
      (* begin out *)
      (*
      Return[    
        ToExpression[ 
          StringReplace[
            ToString[
              InputForm[solution]
            ],
            "recursionOperator`Private`" -> ""
          ]
        ] 
      ], 
      *) 
      (* end out *)
      (* this is else part of if statement *)
      (* WH Aug 24, 2024 added print statement *)
      (* Print["At pt. XXX1, in function RecursionOperator, solution is: "]; *)
      If[recursionOperatorDebug, 
        Print["After testing, PrettyPrint form of the recursion operator is: "]
       ];
      Print[PrettyPrint[ solution ]];
      Return[ CleanUpFunctionWH5[CleanUpFunctionWH4[ InputForm[solution] ] ]],
      Message[RecursionOperator::failed];
      Abort[]
    ] (* end if statement *)
  (* end module RecursionOperator is on line just below *)
  ] /; 
    ( If[Length[equations] == Length[funcs],
        True,
        Message[RecursionOperator::eqns];False] && 
      If[Length[vars] == 2,
        True,
        Message[RecursionOperator::vars];False] && 
      If[
        (And @@ Map[
          (Count[#,q_/; MatchQ[q,Derivative[0,1][_][__]]] === 1 &&
           Count[#,q_/; ! FreeQ[q,Derivative[_,k_/;k>0][_][__]]] === 1)&,
           equations  /. Equal[a_,b_] :> a-b]),
        True,
        Message[RecursionOperator::dt, vars[[2]] ];False] &&
      If[
        FreeQ[ 
          equations,
          Power[a_,b_] /; ( ( Negative[b] || Head[b] === Symbol) && 
            Or @@ (! FreeQ[a, Head[#]]& /@ funcs) )
        ],
        True,
        Message[RecursionOperator::rational];False]
    );  (* end of conditions *)

(* : Title : BuildCandidate *)
(* : Author : Douglas Baldwin *)
(* : Date & Time : Saturday, August 06, 2005 at 12:43 PM *)
(* : Summary : 
  Builds the candidate recursion operator and outputs it in 
  the form needed for DefiningEquation. *)

BuildCandidate[equations_, funcs_, vars_, param_:{}, options___? OptionQ] :=
  Module[{ buildCandidateDebug =  (* Debug bool. *)
            (Verbose /. {options} /. Options[RecursionOperator]),
           weights0, weights, (* The weights assoc. with the dilation sym. *)
           symmetries = {}, (* The first seed + 1 symmetries. *)
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
      BuildCandidate`DetermineWeights[
        equations, funcs, vars, options
      ];
    If[buildCandidateDebug,
      Print["The weights are:"];
      Print[CleanUpFunction[ weights0 ]];
    ];
    (* Puts weights in a more usable form. *)
    (* Specifically: Weight[u] -> 2 goes to {u[x,t], 2} *)
    weights = 
      BuildCandidate`filterWeights[weights0, funcs, vars, options];
    If[buildCandidateDebug,
      Print["The new form of the weights is:"];
      Print[CleanUpFunction[ weights ]];
    ];
    If[weights === {{},{}},
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
        weights,
        Seed /. {options} /. Options[RecursionOperator], 
        options
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
    (* WH Aug 24, 2024 *)
    (* start change *)
    If[buildCandidateDebug,
      Print["The rank of the recursion operator \!\(R\_0\) is:"];
      Print[CleanUpFunction[ rank ]];
    ];
    (* Generate the R0 components. *)
    {maxCoef, candidate0} = 
      BuildCandidate`BuildR0[weights, rank, options ];
    If[buildCandidateDebug,
      Print["Generate the \!\(R\_0\) component: "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[ candidate0 ]]]
      ];
    (* WH Aug 24, 2024 *)
    If[buildCandidateDebug,
      Print["PrettyPrint form of the generated \!\(R\_0\) component: "];
      Print[PrettyPrint[ candidate0 ]];
      Print["Number of terms in operator \!\(R\_0\) is: "];
      (* the line below worked for a scalar recursion operator *)
      (* Print[Length[ Part[candidate0[[1]],1] ]] *)
      Print[ Map[Length, Flatten[ candidate0 ]] ];
      Print["For \!\(R\_0\) in matrix form, these are the number of terms in" <>
      " each of the elements of the matrix (going from left to right, row by row)."] 
      ];  
    (* end change *)
    candidate1 = 
      BuildCandidate`BuildR1[
        # == 0& /@ equations,
        funcs,
        vars,
        rank,
        weights0,
        weights,
        maxCoef,
        options
      ];
    (* WH Jun 24, 2024 *)
    If[buildCandidateDebug,
      Print["Generate the \!\(R\_1\) component: "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[ candidate1 ]]]
    ];
    (* WH Jun 27, 2024 *)
    If[buildCandidateDebug,
      Print["PrettyPrint form of the generated \!\(R\_1\) component: "];
      Print[PrettyPrint[ candidate1 ]];
      Print["Number of terms in operator \!\(R\_1\) is: "];
      (* the line below worked for a scalar recursion operator *)
      (* Print[Length[ Part[candidate1[[1]],1] ]] *)
      Print[ Map[Length, Flatten[ candidate1 ]] ];
      Print["For \!\(R\_1\) in matrix form, these are the number of terms in" <>
      " the elements of the matrix (going from left to right, row by row)."];
      Print["IMPORTANT: Always look at the PrettyPrint forms to see which pieces" <>
      " are under the operators (that is, on the right side of the operator), and" <>
      " which pieces are factors (on the left side of the operator)."]
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
    ]; (* end module BuildCandidate *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

BuildCandidate`getSymmetries[equations_List, funcs_List, vars_List, 
    weights_List, n_Integer, options___] :=
    Module[{ symmDebug = 
            (Verbose /. {options} /. Options[RecursionOperator]),
           i = 0,
           wpars, maxDep,
           symmetries = {}
         }, (* Protected Local Variables *)
   (* Creates a list of rules to remove the weighted parameters from  *)
    (* the equations. *)
    wpars = ((# -> 1 &) /@ 
      (WeightedParameters /. {options} /. Options[RecursionOperator]));
    maxDep = MaxExplicitDependency /. {options} /. Options[RecursionOperator];
    While[ Length[symmetries] < n+1,
      i++;
      symmetries = 
        Union[
          symmetries,
          Internal`DeactivateMessages[
            (* The below is taken from the InvariantsSymmetries package, *)
            (* from the PDESymmetries function. *)
            {#,Integrability`InvariantsSymmetries`Private`pdeSymmetry[
                  equations /. wpars,funcs,vars,weights,#,
                 Integrability`InvariantsSymmetries`MaxExplicitDependency -> maxDep ]
             }& /@ Integrability`InvariantsSymmetries`Private`trialSymRanks[
                    funcs,vars,weights[[1]], 
                    Integrability`InvariantsSymmetries`Private`trialRanks[
                      {i-1,i},Join[weights[[1]],{{_,1}}]
                    ]
                ],
            Solve::svars
          ]
        ] /. {{(_? NumericQ) ..}, {{(0)..}}} :> Sequence[];
      If[symmDebug,
        Print["The symmetries for iteration n = " <> ToString[i] <> " are: "];
        Print[CleanUpFunction[ symmetries ]];
      ];
    ];
    Return[ symmetries ];
  ]; (* end of module BuildCandidate`getSymmetries *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

BuildCandidate`BuildR0[weights_List, rank_List, options___? OptionQ] :=
  Module[{ buildR0Debug = 
            (Verbose /. {options} /. Options[RecursionOperator]),
           coef, (* What form the coefficents are. *)
           ii = 1,  
           sym, symRank, makeRank, maxDep, depTerms, rankShift, genTerms,
           newWeights,
           terms 
         }, (* Protected Local Variables *)
    (* Sets coef to be the name of the unknown coefficient set by the user. *)
    coef = UnknownCoefficients /. {options} /. Options[RecursionOperator];
    (* Sets the maximum explicity dependency on the independent variables. *)
    maxDep = MaxExplicitDependency /. {options} /. Options[RecursionOperator];
    (* A little function which updates the weights of the combined functions. *)
    sym[a_, b_] + sym[c_, d_] ^:= sym[a*c, b + d];
    k_*sym[a_, b_] ^:= sym[a^k, k*b];
    symRank[sym[a_,b_]] := b;
    (* A little function to add enough operatorDs. *)
    makeRank[sym[a_,b_],n_] := 
      DefiningEquation`Operator`Times[operatorD[n - b],a] /; IntegerQ[n-b] && n >= b;
    makeRank[sym[a_,b_],n_] := 0 /; ! IntegerQ[n-b] || n < b;
    (* Converts weights {u[x,t], 2} to sym[u[x,t],2] *)
    newWeights = weights /. {a_, b_?NumericQ} :> sym[a,b];
    (* The maximum dependency terms. *)    
    depTerms = Union[ Flatten[ Table[ i*newWeights[[2]], {i, 0, maxDep}] ] ];
    rankShift = 
      maxDep * Min[symRank /@ depTerms] + 
        (RankShift /. {options} /. Options[RecursionOperator]);
    newWeights = First[newWeights];
    (* A little function to compose the functions to get the right weight. *)
    genTerms[a_,b_] := 
      Flatten[ 
        Table[ # + i*b, 
          {i, 0, Floor[(k-symRank[#]+rankShift)/symRank[b] ]} 
        ]& /@ a 
      ];
    (* Generates all the combinations of functions that are at or below rank. *)
    terms =
      Table[
        ( makeRank[#,rank[[i,j]]+rankShift ]& /@ 
            Flatten[ 
              Table[
                Fold[genTerms, depTerms, newWeights ],
                {k, 0, rank[[i,j]]}
              ]
            ]
        ) /. List -> Plus ,
        {i, Length[rank]},
        {j,Length[rank[[i]] ]}
      ];
(* WH Aug 24, 2024 *)
    If[buildR0Debug,
      Print["The \!\(R\_0\) operator is: "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[ terms ]]];
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
 (* WH SAT 18:02 WH Jul 4, 2024 *)
    If[buildR0Debug,
      Print["The \!\(R\_0\) operator with \!\(C\_i\) attached: "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[ terms ]]]
      ];   
  Return[ {ii, terms} ]
  ]; (* end of module BuildCandidate`BuildR0 *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

BuildCandidate`BuildR1[equations_List, funcs_List,
    vars_List, rank_List, weights0_List, weights_List, maxCoef_Integer,
    options___? OptionQ
  ] :=
  Module[{ buildR1Debug = 
            (Verbose /. {options} /. Options[RecursionOperator]),
           weightUV, 
           wpars,
           maxDep,
           rankShift,
           symmetries,
           invariants,
           cosymmetries,
           R1, ii = maxCoef
         }, (* Protected Local Variables *)
    (* Sets coef to be the unknown coefficient name set by the user. *)
    coef = UnknownCoefficients /. {options} /. Options[RecursionOperator];
    (* Creates a vector of the weights of u. *)
    weightUV = (weight /@ ( Head /@ funcs )) /. First[weights0];
   (* Sets the maximum explicity dependency on the independent variables. *)
    maxDep = MaxExplicitDependency /. {options} /. Options[RecursionOperator];
    rankShift = 
      maxDep * Min[(weight /@ vars) /. First[weights0] ] + 
        (RankShift /. {options} /. Options[RecursionOperator]);
    (* Creates rules to set the weighted parameters equal to 1. *)
    wpars = ((# -> 1 &) /@ 
      (WeightedParameters /. {options} /. Options[RecursionOperator]));
    (* Generates the needed symmetries to build up the non-local part. *)
    symmetries = 
      Internal`DeactivateMessages[
        (* The below is taken from the InvariantsSymmetries package, *)
        (* from the PDESymmetries function. *)
        ({#,Integrability`InvariantsSymmetries`Private`pdeSymmetry[
              equations /. wpars,Head /@ funcs,vars,weights,#,
              Integrability`InvariantsSymmetries`MaxExplicitDependency -> maxDep]
         }& /@ Integrability`InvariantsSymmetries`Private`trialSymRanks[
                Head /@ funcs,vars,weights[[1]], 
                Integrability`InvariantsSymmetries`Private`trialRanks[
                  {0,rank[[1,1]] + rankShift + weightUV[[1]] + 1},Join[weights[[1]],{{_,1}}]
                ]
            ] 
        ) //. {{True}, a_List} :> a,
        Solve::svars
      ] /. C[_] -> 1;
    If[buildR1Debug,
      Print["The symmetries are: "];
      Print[CleanUpFunction[ symmetries ]];
    ];
    invariants = 
      Internal`DeactivateMessages[
        (* The below is taken from the InvariantsSymmetries package, *)
        (* from the PDEInvariants function. *)
        ({{#},Integrability`InvariantsSymmetries`Private`pdeDensity[
              equations /. wpars,Head /@ funcs,vars,weights,#,
              Integrability`InvariantsSymmetries`MaxExplicitDependency -> maxDep]
          }& /@ Integrability`InvariantsSymmetries`Private`trialRanks[
            {0,rank[[1,1]] + rankShift + weightUV[[1]] + 1},Join[weights[[1]],{{_,1}}]
            ]
        ) //. {{True}, a_List} :> a,
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
    Flatten[
      Table[
        Outer[
          DefiningEquation`Operator`Times,
          Transpose[
            { symmetries[[i,2,j]] } 
              /. Times -> DefiningEquation`Operator`Times
          ],
          {{operatorD[-1]}},
          { cosymmetries[[Length[cosymmetries] - i + 1, 2, k]] }
            /. Times -> DefiningEquation`Operator`Times
        ],
        {i, Length[symmetries]},
        {j, Length[ symmetries[[i,2]] ]},
        {k, Length[ cosymmetries[[Length[cosymmetries] - i + 1, 2]] ]}
      ],
      2
    ];
    (* WH Aug 24, 2024 *)
    (* Put it in the correct form and adds coef. *)
    R1 = 
      ( Plus @@ Union[Flatten /@ #& /@ R1] ) /. 
        DefiningEquation`Operator`Times[a__] :>
          DefiningEquation`Operator`Times[coef[ii++],a,operatorD[0]];
    If[buildR1Debug,
      Print["The \!\(R\_1\) operator is:"];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[ R1 ]]];
    ];
    Return[ R1 ]
  ]; (* end of module BuildCandidate`BuildR1 *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

(* : Title : Calculate Weights. *)
(* : Author : Douglas Baldwin *)
(* : Date & Time : Saturday, August 06, 2005 at 12:43 PM *)
(* : Summary : Computes the dilation symmetry of a system of PDEs. *)

BuildCandidate`DetermineWeights[eqns_List, funcs_List, 
    vars_List, options___? OptionQ] :=
  Module[{ weightsDebug = WeightsVerbose /. {options} /. Options[RecursionOperator],
           equations,
           termWeights, 
           variables,
           weightRules,
           weights
         }, (* Protected Local Variables *)
    (* Put equations in the right form. *)
    equations = MapAll[Expand,eqns] /. Equal[a_,b_] :> a-b ;
    (* Form a list of the terms. *)
    termWeights = 
      determineWeights`ListFormation[
        equations,funcs,vars,options
      ];
    If[weightsDebug,
      Print["The weight terms are:"];
      Print[CleanUpFunction[ termWeights ]];
    ];
    (* Solves for the weights. *)
    weights = 
      determineWeights`Solver[termWeights, funcs, vars, options];
    If[weightsDebug,
      Print["The weights are: "];
      Print[CleanUpFunction[ weights ]];
    ];
   (* Normalizes the weights. 
       DB:8/4/2005 Removed after setting weight[x]=weight[y]=-1
    If[ weights =!= {{},{}},
      weights = 
        determineWeights`NormalizeWeights[weights,vars]
    ]; *)
    If[weightsDebug,
      Print["The weights, after being normalized:"];
      Print[CleanUpFunction2[ weights ]];
    ];
    Return[ weights ];
 ]; (* end of module BuildCandidate`DetermineWeights *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

determineWeights`ListFormation[equations_List, funcs_List, 
    vars_List, options___? OptionQ] :=
  Block[{ listFormationDebug = 
            WeightsVerbose /. {options} /. Options[RecursionOperator],
          weight, terms
         }, (* Protected Local Variables *)
    If[listFormationDebug,
      Print["equations:"];
      Print[CleanUpFunction2[ equations ]];
    ];
    (* Replaces functions and their derivatives by E^(weight expression) *)
    terms = 
      equations /. 
        { f_[x__] :> E^weight[f] /; MemberQ[Head /@ funcs, f],
          Derivative[n__][f_][x__] :>
            E^(weight[f] - Plus @@ ( Times @@ #& /@ 
                Transpose[{weight /@ {x}, {n}}]))
              /; MemberQ[Head /@ funcs, f],
          x_ :> E^weight[x] /; 
            MemberQ[WeightedParameters /. {options} /. Options[RecursionOperator], x],
          x_ :> E^weight[x] /; MemberQ[vars, x],
          Plus -> List
        };
    If[listFormationDebug,
      Print["terms, with exponetial weights:"];
      Print[CleanUpFunction2[ terms ]];
    ];
    (* Extracts the terms in the exponential and puts it in a List. *)
    terms = terms /. ( _ : 1)*E^(a_) :> a;
    If[listFormationDebug,
      Print["terms, with weights extracted:"];
      Print[CleanUpFunction2[ terms ]];
    ];
   (* Sort the list, so those terms with t-derivatives are first. *)
    terms = 
      Sort[#, 
        (! FreeQ[#1, weight[Last[vars]]] && FreeQ[#2,  weight[Last[vars]]]) &
      ]& /@ terms;
    If[listFormationDebug,
      Print["terms, sorted:"];
      Print[CleanUpFunction2[ terms ]];
    ];
   (* Return the result. *)
    Return[ terms ]
  ]; (* end of block determineWeights`ListFormation *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

determineWeights`Solver[weightList_, funcs_, vars_, options___? OptionQ]:=
  Module[{ solverDebug = WeightsVerbose /. {options} /. Options[RecursionOperator],
           nonzeros,
           solveFor,
           eqnList,
           weights (* The solution. *)
         }, (* Protected local variables. *)
    (* The terms with nonzero weight. *)
    nonzeros = 
      weight /@ Join[Head /@ funcs, Take[vars,-1]];
    (* The terms to solve for. *)
    solveFor = 
      weight /@ 
        Join[ 
          Join[vars, Head /@ funcs],
          ( WeightedParameters /. {options} /. Options[RecursionOperator])
        ];
    (* Forms a list of equations to be solved for weights. *)
    eqnList = 
      Union[
        Flatten[
          Thread[Equal[First[#],Rest[#]]]& /@ weightList
        ]
      ]; 
    If[solverDebug,
      Print["The Equations to be solved first"];
      Print[CleanUpFunction[eqnList]];
    ];    
    (* Does the first run of solving. *)
    Check[ (* Block surpress messages, but still allows Check to*)
      Block[{$Messages = {}}, (* check for Solve:svars. By *)
        weights = 
          Flatten[  (* checking for svars, I know it found all *)
            Solve[  (* the weights I needed it to. *)
              Union[
                eqnList, 
                (# != 0)& /@ nonzeros,
                (weight[#] == -1)& /@ Drop[vars, -1],
                ( WeightRules /. {options} /. 
                  Options[RecursionOperator] /. Rule -> Equal )
              ],
              solveFor
            ]
          ]
        ],
      Message[weight::nonuniform3,CleanUpFunction2[ weights ]];
      Abort[];,
      Solve::svars
    ];
    If[solverDebug,
      Print["weights:"];
      Print[CleanUpFunction2[ weights ]];
    ];
    If[ weights =!= {} || Length[eqnList] === 1, 
      Return[{(# -> (# /. weights /. 
        ( WeightRules /. {options} /. Options[RecursionOperator])))& /@ 
          Union[solveFor, weight /@ vars],{}}],
      Return[
        determineWeights`nonuniformSolver[eqnList, nonzeros, solveFor,vars,options] 
      ]
    ]
  ]; (* end of module determineWeights`Solver *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

determineWeights`nonuniformSolver[weightExpr_List, nonzeros_, solveFor_, 
    vars_, options___] :=
  Module[{ nonuniformDebug = WeightsVerbose /. {options} /. Options[RecursionOperator],
           nextSubset, 
           currentSet = {First[weightExpr]},
           weights = {}, (* solutions *) 
           lefts,rights
         }, (* Protected Local Variables *)
    (* Modified from Combinatorica by Steven S. Skiena *)
    nextSubset[set_List, subset_List] := 
      {} /; (Take[set, -Length[set]+1] === subset);
    nextSubset[set_List, subset_List] := 
      Take[set, Length[subset]+1] /; (Take[set, -Length[subset]] === subset);
    nextSubset[set_List, subset_List] := 
      Module[{h = 1, x = 1}, 
        While[set[[-h]] == subset[[-h]], h++];
        While[set[[x]] =!= subset[[-h]], x++];
        Join[Drop[subset, -h], Take[set, {x + 1, x + h}]]
      ];
    (* Print message to inform user that the weights are not uniform. *)
    Message[weight::nonuniform1];
    (* Begin while loop to find if adding one or more parameter *)
    (* can resolve the nonuniform weight structure.  Rather than add *)
    (* weighted parameters (which would imply solving nonlinear *)
    (* equations), this is done by removing relations. *)
    (* The below code is based on the makeUniform from the *)
    (* InvariantsSymmetries.m package by Unal Goktas and Willy Hereman *)
    While[ currentSet =!= {},
      If[nonuniformDebug,
        Print["currentSet:"];
        Print[CleanUpFunction2[ currentSet ]];
      ];
      (* Solves for the weights, but if Solve cannot "solve" for all *)
      (* the variables, then it returns {}. *)
      Check[ (* Block surpress messages, but still allows Check to*)
        Block[{$Messages = {}}, (* check for Solve:svars. By *)
          weights = 
            Flatten[  (* checking for svars, I know it found all *)
              Solve[  (* the weights I needed it to. *)
                Union[
                  Complement[weightExpr, currentSet], 
                  (# != 0)& /@ nonzeros,
                  (weight[#] == -1)& /@ Drop[vars, -1],
                  ( WeightRules /. {options} /. 
                    Options[RecursionOperator] /. Rule -> Equal )
                ],
                solveFor
              ]
            ]
          ],
        Message[weight::nonuniform3,CleanUpFunction2[ weights ]];
        weights = {},
        Solve::svars
      ];
      If[nonuniformDebug,
        Print["weights:"];
        Print[CleanUpFunction2[weights]];
      ];
      (* Checks that the weight[u],weight[v] are positive. *)
        If[ 
          Count[weights, 
            q_/; MemberQ[funcs,q[[1,1]]] && q[[2]] < 0
          ] > 0,
          weights = {}
        ];
        (* If there is a solution, then it computes the weights *)
        (* of the parameters and checks that they are positive. *)
        If[weights =!= {},
          {lefts,rights} = 
            {#[[1]]& /@ currentSet, #[[2]]& /@ currentSet} /. weights;
          If[
            And @@ ( Thread[lefts > rights] ),
            Message[weight::nonuniform2];
            Return[{(# -> (# /. weights /.  ( WeightRules /. {options} /. 
                Options[RecursionOperator])))& /@ Union[solveFor, weight /@ vars],
              Union[lefts-rights]}], 
            weights = {}
          ]
        ];
      currentSet = nextSubset[weightExpr, currentSet]
    ];
    Message[weight::nonuniform4]; 
    Return[ {{},{}} ]
  ]; (* end of module determineWeights`nonuniformSolver *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

determineWeights`NormalizeWeights[weights_List, vars_List] :=
  Module[{ normalizedScalingFactor,
           normalizedWeights
         }, (* Protected Local Variables *)
      normalizedScalingFactor = 
      PolynomialGCD @@ Flatten[ { weights[[1]] /. Rule[_,a_] :> a, weights[[2]] } ];
      normalizedWeights = 
      { weights[[1]] /. Rule[a_,b_] :> Rule[a,Divide[b,normalizedScalingFactor]],
        Divide[weights[[2]],normalizedScalingFactor]
      };
    If[
      FreeQ[Flatten[normalizedWeights /. Rule[_,a_] :> a], weight],
      Return[normalizedWeights],
      Message[weight::needMoreInformation,
        CleanUpFunction2[normalizedWeights]];
      Return[ {{},{}} ]
    ]
  ]; (* end of module determineWeights`NormalizeWeights *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

(* Takes weight[u] -> 2 to {u[x,t], 2} and reorders. *)
BuildCandidate`filterWeights[weights_, funcs_, vars_, options___] :=
  Module[{ scalingSymmetry, scalingSymmetryFuncs, scalingSymmetryVars, 
           scalingSymmetryWPars, wPars
         }, (* Protected Local Variables *)
      (* List of the weighted parameters. *)
      wPars = 
        WeightedParameters /. {options} /. Options[RecursionOperator];
      (* Takes weight[u] -> 2 to {u[x,t], 2} *)
      scalingSymmetry = 
        Union[weights[[1]] /. Rule[weight[a_], b_] :> {a,b},
          {1,#}& /@ weights[[2]]
        ] /. ((# -> 1 &) /@ wPars);
      (* Picks off the scaling symmetries of the functions. *)
      scalingSymmetryFuncs = 
        Cases[
          scalingSymmetry, 
          a_/; MemberQ[Head /@ funcs, a[[1]]]
        ] /. a_/; MemberQ[Head /@ funcs,a] -> a[Sequence @@ vars];
      (* Picks off the weighted parameter scaling symmetries. *)
      scalingSymmetryWPars = 
        Cases[
          scalingSymmetry,
          a_/; NumberQ[a[[1]]] 
        ];
      (* Picks off the scaling symmetries of the variables. *)
      scalingSymmetryVars = 
        Cases[scalingSymmetry, a_/; MemberQ[vars,a[[1]]]];
      (* Combines and reorders. *)
      scalingSymmetry = 
        Reverse[
          Sort[
            Union[scalingSymmetryFuncs,scalingSymmetryWPars],
            OrderedQ[{#1[[2]],#2[[2]]}]&
          ]
        ];
      (* Returns results. *)
      Return[ {scalingSymmetry,scalingSymmetryVars} ]
  ]; (* end of module BuildCandidate`filterWeights *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

(* : Title : Defining Equation *)
(* : Author : Douglas Baldwin *)
(* : Date : Friday, August 05, 2005 at 9:57 AM *)

DefiningEquation[system_, operator_, funcs_, vars_, 
    param_:{}, options___ ] := 
  DefiningEquation[
    If[ Head[#] === List, #, {#}]&[system], 
    If[ Head[#] === List, #, {{#}}]&[operator], 
    If[ Head[#] === List, #, {#}]&[funcs], 
    If[ Head[#] === List, #, {#}]&[vars],
    If[ Head[#] === List, #, {#}]&[param],
    options
  ]; (* end of function definingequation *)

DefiningEquation[system_List, operator_, funcs_List, vars_List, 
    param_List, options___ ] :=
  Module[{primaryDebug = 
            (Verbose /. {options} /. Options[RecursionOperator]),
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
    (* WH Aug 24, 2024 *)
    (* The following prints the input given by the user. *)
    If[primaryDebug,
      Print["The system entered by the user (called system):"];
      (* Print[CleanUpFunction[system]]; *)
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[system]]];
      Print["The operator entered by the user (called operator):"];
      (* Print[CleanUpFunction[operator]]; *)
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[operator]]];
      Print["The dependent variables are the functions (called funcs):"];
      (* Print[CleanUpFunction[ funcs ]]; *)
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[funcs]]];
      Print["The independent variables (called vars):"];
      (* Print[CleanUpFunction[ vars ]]; *)
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[vars]]];
      Print["The parameters are (called param):"];
      (* Print[CleanUpFunction[ param ]]; *)
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[param]]];
    ]; (* end if *)
    (* Converting the form of the operator for internal calculations. *)
    (* DB:1/4/2004: Added If. *)
    newOperator = 
      DefiningEquation`Operator`Expand[
        If[ FreeQ[operator, operatorD],
          DefiningEquation`ToOperator /@ #& /@ operator,
          operator
        ]
      ];
    (* WH Aug 24, 2024 *)
    If[primaryDebug,
      Print["This is the operator \[ScriptCapitalR]: "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[newOperator]]]; 
      (* WH Aug 24, 2024 taken out TeXPrint form below *)
      (* DefiningEquation`TeXPrint[newOperator]; *)
      Print["PrettyPrint form of the operator \[ScriptCapitalR]: "];
      Print[PrettyPrint[ newOperator ]];
      Print["Number of terms in operator \[ScriptCapitalR] is: "];
      (* the line below worked for a scalar recursion operator *)
      (* Print[Length[ Part[newOperator[[1]],1] ]] *)
      Print[ Map[Length, Flatten[ newOperator ]] ];
      Print["For \[ScriptCapitalR] in matrix form, these are the number of terms in" <>
      " the elements of the matrix (going from left to right, row by row)."] 
      ];
    (* DB:8/5/2005 changed t to Last[vars]. *)
    partialOperatorOverPartialT = 
      (
        newOperator /. 
          { DefiningEquation`Operator`Times[a___, Last[vars], b___] :> 
              DefiningEquation`Operator`Times`Temp[a,b],
            DefiningEquation`Operator`Times[a___, Power[Last[vars], n_], b___] :> 
              DefiningEquation`Operator`Times`Temp[n, a, Power[Last[vars], n-1], b],
            DefiningEquation`Operator`Times[a__] :> 0 /;
              FreeQ[{a}, Last[vars], 1]
          }
      ) /. DefiningEquation`Operator`Times`Temp -> 
              DefiningEquation`Operator`Times;
    (* WH Aug 24, 2024 *)
    If[primaryDebug,
      Print["Partial derivative of the operator with respect to t, " <>
        "\!\(\( \[PartialD]\[ScriptCapitalR] \) \/ \( \[PartialD] t \)\)"
      ];
      Print[CleanUpFunction[partialOperatorOverPartialT]];
    ];
    (* We then calculate the Frechet Derivative of the system F(u). *)
    frechetDerivativeOfSystem = 
      DefiningEquation`Operator`Expand[
        DefiningEquation`System`FrechetDerivative[system]
      ];
    (* WH Jun 24, 2024 *)
    (* WH Aug 24, 2024 *)
    If[primaryDebug,
      Print["The Frechet derivative F'(u): "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[frechetDerivativeOfSystem]]]
      (* WH Aug 24, 2024 taken out TeXPrint form below *)
      (* DefiningEquation`TeXPrint[frechetDerivativeOfSystem] *)
    ];
    (* WH Jun 27, 2024 *)  
    (* WH Aug 24, 2024 *)
    If[primaryDebug,
      Print["PrettyPrint form of the Frechet derivative, F'(u): "];
      Print[PrettyPrint[frechetDerivativeOfSystem]];
      Print["Number of terms in operator F'(u) is: "];
      (* the line below worked for a scalar recursion operator *)
      (* Print[Length[ Part[frechetDerivativeOfSystem[[1]],1] ]] *)
      Print[ Map[Length, Flatten[ frechetDerivativeOfSystem ]] ];
      Print["For F'(u) in matrix form, these are the number of terms in" <>
      " the elements of the matrix (going from left to right, row by row)."] 
      ];
    (* We then Compute $R \cirdot F'(u)$ *)
    (* WH Jul 4, 2024 *)
    (* WH Aug 24, 2024 *)
    If[primaryDebug,
       Print["Entering function operatorComposedWithFrechetDerivativeOfSystem "];
       Print[" with operator = "];
       Print[PrettyPrint[newOperator]]
      ];
    operatorComposedWithFrechetDerivativeOfSystem = 
      DefiningEquation`Operator`Expand[
        Inner[
          DefiningEquation`Operator`ExpressionMultiply,
          newOperator,
          frechetDerivativeOfSystem,
          Plus
        ]
      ]; (* end expand *)
(* WH Jun 24, 2024 *)
   If[primaryDebug,
      Print["Composition \[ScriptCapitalR]\[SmallCircle]F'(u): "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[
           operatorComposedWithFrechetDerivativeOfSystem]]]
      (* WH Aug 24, 2024 taken out TeXPrint form below *)
      (* DefiningEquation`TeXPrint[operatorComposedWithFrechetDerivativeOfSystem]; *)
    ];
    (* WH Jun 27, 2024 *)  
    (* WH Aug 24, 2024 *)
    If[primaryDebug,
      Print["PrettyPrint form of the composition \[ScriptCapitalR]\[SmallCircle]F'(u): "];
      Print[PrettyPrint[operatorComposedWithFrechetDerivativeOfSystem]];
      Print["Number of terms in \[ScriptCapitalR]\[SmallCircle]F'(u) is: "];
      (* the line below worked for a scalar recursion operator *)
      (* Print[Length[ Part[operatorComposedWithFrechetDerivativeOfSystem[[1]],1] ]] *)
      Print[ Map[Length, Flatten[ operatorComposedWithFrechetDerivativeOfSystem ]] ];
      Print["For \[ScriptCapitalR]\[SmallCircle]F'(u) in matrix form, these are the number of" <>
      " terms in each of the elements of the matrix (going from left to right, row by row)."] 
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
      ]; (* end expand *)
    (* WH Jun 24, 2024 *)
    (* WH Aug 24, 2024 *)
    If[primaryDebug,
      Print["Entering function frechetDerivativeOfSystemComposedWithOperator"];
      Print[" with operator = "];
      Print[PrettyPrint[newOperator]]; 
      Print["Composition F'(u)\[SmallCircle]\[ScriptCapitalR]: "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[
            frechetDerivativeOfSystemComposedWithOperator]]]
      (* WH Aug 24, 2024 taken out TeXPrint form below *)      
      (* DefiningEquation`TeXPrint[frechetDerivativeOfSystemComposedWithOperator] *)
    ];
    (* WH Jun 27, 2024 *)  
    (* WH Aug 24, 2024 *)
    If[primaryDebug,
      Print["PrettyPrint form of the composition F'(u)\[SmallCircle]\[ScriptCapitalR]: "];
      Print[PrettyPrint[frechetDerivativeOfSystemComposedWithOperator]];
      Print["Number of terms in F'(u)\[SmallCircle]\[ScriptCapitalR] is: "];
      (* the line below worked for a scalar recursion operator *)
      (* Print[Length[ Part[frechetDerivativeOfSystemComposedWithOperator[[1]],1] ]] *)
      Print[ Map[Length, Flatten[ frechetDerivativeOfSystemComposedWithOperator ]] ];
      Print["For F'(u)\[SmallCircle]\[ScriptCapitalR] in matrix form, these are the number of" <>
      " terms in each of the elements of the matrix (going from left to right, row by row)."] 
    ]; 
    (* WH Aug 24, 2024 *)
    If[primaryDebug,
      Print["Entering routine DefiningEquation`Operator`System`FrechetDerivative"];
      Print[" with system = "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[system]]];     
      Print["Entering routine DefiningEquation`Operator`System`FrechetDerivative"];
      Print[" with operator = "];
      Print[PrettyPrint[newOperator]]
      ];
    (* WH Jul 4, 2024 since the computation does not work *)
    (* give the result computed separately interactively *)
    frechetDerivativeOfOperator = 
      DefiningEquation`Operator`Expand[
        DefiningEquation`Operator`System`FrechetDerivative[
          system,
          newOperator
        ]
      ]; 
    (* WH Jun 24, 2024 *)
    (* WH Aug 24, 2024 *)
    If[primaryDebug,
      Print["Frechet derivative of the operator \[ScriptCapitalR]'[F]: "];
      Print[CleanUpFunctionWH3[CleanUpFunctionWH2[frechetDerivativeOfOperator]]]
      (* WH Aug 24, 2024 taken out TeXPrint form below *)
      (* DefiningEquation`TeXPrint[frechetDerivativeOfOperator] *)
     ];
    (* WH Jun 27, 2024 *) 
    (* WH Aug 24, 2024 *) 
    If[primaryDebug,
      Print["PrettyPrint form of the Frechet derivative of the operator \[ScriptCapitalR]'[F]: "];
      Print[PrettyPrint[frechetDerivativeOfOperator]];
      Print["Number of terms in \[ScriptCapitalR]'[F] is: "];
      (* the line below worked for a scalar recursion operator *)
      (* Print[Length[ Part[frechetDerivativeOfOperator[[1]],1] ]] *)
      Print[ Map[Length, Flatten[ frechetDerivativeOfOperator ]] ];
      Print["For \[ScriptCapitalR]'[F] in matrix form, these are the number of terms" <>
      " in each of the elements of the matrix (going from left to right, row by row)."] 
      ]; 
    definingEquationResult = 
        DefiningEquation`Operator`Constraints[
          partialOperatorOverPartialT 
          + frechetDerivativeOfOperator 
          + operatorComposedWithFrechetDerivativeOfSystem
          - frechetDerivativeOfSystemComposedWithOperator,
          param, options
        ];
     (* TO DO TO FIX WH Aug 24, 2024 what to do with TeXPrint form below ??? *)
    If[definingEquationResult =!= {True, {}},
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
    ]
  ]; (* end of module DefiningEquation *)
 
(* : Title : Operator Times *)
(* : Author : Douglas Baldwin *)
(* : Date : Wednesday, July 10, 2002 *)

(* TO DO TO FIX WH Aug 24, 2024. Is clearing of operatorTimes needed? *)
(* For testing purposes, I clear operatorTimes. *)
ClearAll[DefiningEquation`Operator`Times];

(* Sets up zero identity. *)
DefiningEquation`Operator`Times[___, 0, ___] := 0;

(* Sets up one identity. *)
DefiningEquation`Operator`Times[a___, 1, b___] := 
    DefiningEquation`Operator`Times[a, b];

(* Defines the distributive property for operator times. *)
(* DB:8/6/2005, replacing with Distribute function. 
DefiningEquation`Operator`Times[a___] :=
  ( ( DefiningEquation`Operator`Times @@ ({a} /. Plus -> List)
    ) /. List -> Plus
  ) /; (! FreeQ[{a}, Plus]);
*)
DefiningEquation`Operator`Times[a___,b_,c___] :=
  ( ( DefiningEquation`Operator`Times[a,b /. Plus -> List, c]
    ) /. List -> Plus
  ) /; (! FreeQ[{b}, Plus]);

(* Defines the multiplication of derivative operators. *)
DefiningEquation`Operator`Times[a___, operatorD[n_], operatorD[m_], b___] := 
    DefiningEquation`Operator`Times[a, operatorD[n + m], b] /; (n =!= -1);

DefiningEquation`Operator`Times[a___, operatorD[n_], operatorD[m_]] := 
    DefiningEquation`Operator`Times[a, operatorD[n + m] ];

(* Allows the identity to shift to the end of the expression. *)
DefiningEquation`Operator`Times[a___, operatorD[0], b__] := 
    DefiningEquation`Operator`Times[a, b, operatorD[0]];

(* Defines the actions of the operator derivative. *)
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
  ]; (* end of module DefiningEquation`Operator`Times *)

(* Defines the actions of the operator integral. *)
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
      ]
    ]; (* end if *)
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
    ] (* end of return *)
  ] /; FreeQ[{u}, operatorD];
(* end of module DefiningEquation`Operator`Times in line above *)

(* Defines a frechet derivative for an operator expression *)
DefiningEquation`Operator`Derivative[terms_List, expression_, function_] :=
  Module[{ derivativeDebug = False,
           g = terms, n = 0, h = {1}, dF, 
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
             DefiningEquation`Operator`Times[___, operatorD[n_] ] :> 0 /; 
               n =!= 0
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
    frechetDerivative = 
      DefiningEquation`Operator`Expand[ 
        frechetDerivative
      ];
    If[derivativeDebug,
      Print["After expanding, frechetDerivative = ",
        CleanUpFunction[frechetDerivative],
        " for expression, ",
        CleanUpFunction[expression ],
        ", and function, ",
        CleanUpFunction[function ],
        "."
      ];
    ];
  Return[ frechetDerivative ]
  ]; (* end of module DefiningEquation`Operator`Times *)

(* Sets the attributes of the operator times funciton *)
SetAttributes[DefiningEquation`Operator`Times, {Flat, Listable}];

(* Sets attributes for DefiningEquation`Operator`Times`Temp *)
(* for testing purposes. *)
SetAttributes[DefiningEquation`Operator`Times`Temp, {Flat, Listable}];

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
      ]; (* end of replaceall *)
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
      Print["After term grouping, result is "];
      Print[CleanUpFunction[ result /. Times -> 
      DefiningEquation`Operator`Times]];
     ];
	  result = result /. Times -> DefiningEquation`Operator`Times;
		(* DB:6/23/2007 *)
		result = result /.
			DefiningEquation`Operator`Times[a___] :> 
				DefiningEquation`Operator`Times @@ ({a} /. 
					DefiningEquation`Operator`Times -> Times);
    Return[ result ]
   ]; (* end of module DefiningEquation`Operator`Expand *)

(* : Title : Operator Factor *)
(* : Author : Douglas Baldwin *)
(* : Date : Saturday, August 06, 2005 at 7:28 PM *)

DefiningEquation`Operator`Constraints[system_, param_, options___? OptionQ] :=
  Module[{ factorDebug = 
            (Verbose /. {options} /. Options[RecursionOperator]),
           result
         },
    (* The first step is to expand the expression. *)
    result = 
      DefiningEquation`Operator`Expand[system];
    (* WH Aug 24, 2024 *)
    If[factorDebug,
      Print["--- Derivation linear system for undetermined coefficients ---"];
      Print[" --- Finding conditions on parameters (if present) --- "];
      Print["After substitution of the operator into the defining equation the result is:  "];
      Print[CleanUpFunction[CleanUpFunctionWH3[CleanUpFunctionWH3[ result ]]]];
      Print["PrettyPrint form of the result: "];
      Print[PrettyPrint[ result ]];
      Print["Number of terms in result is: "];
      (* the line below worked for a scalar recursion operator *)
      (* Print[Length[ Part[result[[1]],1] ]] *)
      Print[ Map[Length, Flatten[ result ]] ];
      Print["For the case of a matrix, these are the number of terms in" <>
      " the elements of the matrix (going from left to right, row by row)."] 
      ];
    (* WH Jun 24, 2024 *)
    (* If everything simplifies, it returns True. *)
    If[ Union[ Flatten[ result ] ] === {0},
      (* WH Aug 24, 2024 *)
      If[factorDebug,
         Print["The defining equation was satisfied!"];
         Print[CleanUpFunction[CleanUpFunctionWH3[CleanUpFunctionWH3[ {True, {} } ]]]]
      ];
      Return[ {True, {}} ]
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
      Print["The variables to solve for: "];
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
      ]; (* end map all *)
    If[factorDebug,
      Print["The equations to be solved: "];
      Print[CleanUpFunction[ result ]];
    ];     
    (* DB:1/5/2004 Added the removal of True. *)
    result = 
      ( Flatten /@ 
        analyzeSystem[result, theVariables, param]
      ) /. True :> Sequence[];
    If[factorDebug,
      Print["Result after solving for the variables: "];
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
  end removed by DB *)
  (* WH Aug 24, 2024 *)
  (* WH Aug 24, 2024 added print statement *)
  (*  
    Print["At pt. XXX2, in function DefiningEquation`Operator`Constraints, result is: "];
    Print[PrettyPrint[ result ]];
  *)
 (* back to what it was before *)
 Return[ result ]
 (* tried to replace this by *)
 (*    
 Return[{ result, PrettyPrint[result] } ] 
 *)
  ] /; Head[system] === List;
(* end of module DefiningEquation`Operator`Constraints on previous line *)

DefiningEquation`Operator`Constraints[0] = {};

DefiningEquation`Operator`Constraints[expression_] :=
  Module[{ factorDebug = False,
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
end removed by Unal *)
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
    Return[ result ]
    ] /; Head[expression] =!= List;
(* end of module DefiningEquation`Operator`Constraints in line above *)

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
        Return[{Table[unknown[[i]] -> 0, {i, Length[unknown]}]}]
      ]
  ]; (* end of module analyzeSystem *)

myLength[expr_]:= If[Head[expr] === Plus,Length[expr],1]

(* Uses separator *)
buildEquationList[expr_,funcs_,vars_] :=
    Module[
      {nexpr = Expand[expr],eqlist = {},list1,list2,same,i,j},
      (* DB:8/5/2005 Added u =!= Times. *)
      eqlist = {eqlist,nexpr /. u_[_,_] :> 0 /; u =!= Times };
      nexpr = nexpr-(nexpr /. u_[_,_] :> 0 /; u =!= Times );
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
    ]; (* end of module buildEquationList *)

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
  ]; (* end of module separator *)

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
    Return[ newOperator ]
  ]; (* end of block DefiningEquation`ToOperator *)

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
    Return[ frechetDerivativeOfSystem ]
  ]; (* end of block DefiningEquation`System`FrechetDerivative *)

(*---------------------------------------------------------------------------*)

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
    Return[ frechetDerivative ]
  ]; (* end of block DefiningEquation`Expression`FrechetDerivative *)

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
      Print[CleanUpFunction[frechetDerivativeOfSystem]]
      (* WH Aug 24, 2024 taken out TeXPrint form below *)
      (* DefiningEquation`TeXPrint[frechetDerivativeOfSystem]; *)
    ];
    Return[ frechetDerivativeOfSystem ]
  ]; (* end of module DefiningEquation`Operator`System`FrechetDerivative *)

(*---------------------------------------------------------------------------*)

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
					(* DB:6/23/2007 Changed from *)
					(* DefiningEquation`Operator`Times -> DefiningEquation`Operator`D *)
					DefiningEquation`Operator`Times[a___] :> 
						DefiningEquation`Operator`D @@ ({a} /. 
							DefiningEquation`Operator`Times -> Times),
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
    Return[ frechetDerivative ]
  ]; (* end of module DefiningEquation`Expression`FrechetDerivative *)

(*---------------------------------------------------------------------------*)
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
    	(* DB:6/23/2007 In case Operator`Times is inside Operator`Times *)
		result = result /.
			DefiningEquation`Operator`Times[a___] :> 
				DefiningEquation`Operator`Times @@ ({a} /. 
					DefiningEquation`Operator`Times -> Times);
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
    Return[ result ]
  ]; (* end of block DefiningEquation`Operator`ExpressionMultiply *)

(* : Title : CleanUpFunction *)
(* : Author : Douglas Baldwin *)
(* : Date : Sunday, June 23, 2002 *)
(* : Description : A little function to clean up the output before *)
(*   it's sent to the user. *)

(* CHANGES BY UG -- Jun 27, 2024 *)
(* fixing the CleanUpFunction per WH's request -- UG -- June 27, 2024*)
(* commenting out the old version of CleanUpFunction *)
(* start taken out by UG *)
(*
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
*)
(* end taken out by UG *)

(* now the new version of CleanUpFunction -- UG -- June 27, 2024 *)
CleanUpFunction = 
  (
  If[$VersionNumber < 7.0,
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
      },
      (* if $VersionNumber >= 7.0 *)
      ToExpression[ 
      StringReplace[
        ToString[
          InputForm[#]
        ],
        "recursionOperator`Private`" -> ""
      ]
    ] //. 
    { Derivative[n__][u_[a_,b_]][x__] :>
         Subscript[u, StringTemplate["(`a`,`b`)"][<|"a" -> a, "b" -> b|>], 
            Row @@ {Flatten[
            Table[#[[1]], {#[[2]]}] & /@ Transpose[{{x}, {n}}]]}],
      Derivative[n__][F[a_]][x__] :>
        Subscript[u, a, 
           Row @@ {Flatten[
           Table[#[[1]], {#[[2]]}] & /@ Transpose[{{x}, {n}}]]}],
      Derivative[n__][F_[a_]][x__] :>
        Subscript[F, a, 
           Row @@ {Flatten[
           Table[#[[1]], {#[[2]]}] & /@ Transpose[{{x}, {n}}]]}],
      Derivative[n__][g_][x__] :>
        Subscript[g,
          Row @@ {Flatten[
          Table[#[[1]], {#[[2]]}] & /@ Transpose[{{x}, {n}}]]}],
      F[a_][__] :> Subscript[u, a],
      u[a_, b_][__] :>
        Subscript[u, StringTemplate["(`a`,`b`)"][<|"a" -> a, "b" -> b|>]],
      u[a_, b_] :> 
        Subscript[u, StringTemplate["(`a`,`b`)"][<|"a" -> a, "b" -> b|>]]
      }
  ]
  ) &;
(* end of CleanUpFunction in line above *)

(* begin added by WH Aug 24, 2024 *)
(* WH Jun 24, 2024 CleanUpFunctionWH definition *)
CleanUpFunctionWH1 = 
  ToExpression[
    StringReplace[ToString[InputForm[#]],
      "DefiningEquation`Operator`"->""
    ]
  ]& ;
  (* end added by WH - Aug. 24, 2024 *)
  
(* WH Jun 24, 2024 CleanUpFunctionWH definition *)
CleanUpFunctionWH2 = 
  ToExpression[
    StringReplace[ToString[InputForm[#]],
      "recursionOperator`Private`"->""
    ]
  ]& ;
  (* end added *)  
 DefiningEquation`Operator 

(* WH Jun 24, 2024 CleanUpFunctionWH definition *)
CleanUpFunctionWH3 = 
  ToExpression[
    StringReplace[ToString[InputForm[#]],
      "DefiningEquation`Operator`"->""
    ]
  ]& ;
  (* end added *)  

(* WH Jun 24, 2024 CleanUpFunctionWH definition *)
CleanUpFunctionWH4 = 
  ToExpression[
    StringReplace[ToString[InputForm[#]],
      "DefiningEquation`Operator`"->""
    ]
  ]& ;
  (* end added *)  
  
(* WH Jun 24, 2024 CleanUpFunctionWH definition *)
CleanUpFunctionWH5 = 
  ToExpression[
    StringReplace[ToString[InputForm[#]],
      "recursionOperator`Private`"->""
    ]
  ]& ;
  (* end added *) 

(* WH Jun 24, 2024, CleanUpFunction2 definition *)
CleanUpFunction2 = 
  ToExpression[ 
    StringReplace[
      ToString[
        InputForm[#]
      ],
      "recursionOperator`Private`" -> ""
    ]
  ]&
(* end added by WH Aug 24, 2024 *)

(* : Title : Pretty Print *)
(* : Author : Douglas Baldwin *)
(* : Date : Monday, July 01, 2002 *)
(* : Description : A little function to clean up the output before *)
(*   it's sent to the user. *)

(* NOTES WH Jun 25, 2024 *)
(* Pretty Print might show the correct structure of the operators. *)
(* For example terms like u_x D_x^{-1} u I should be shown as *)
(* u_x operatorD[-1]*u*operatorD[0] and not as operatorD[-1]*u*operatorD[0]*u_x *)
(* otherwise it would be unclear whether or not the factor u_x is UNDER *)
(* the integral operator D_x^{-1} *)

(* CHANGES BY UG -- Jun 27, 2024 *)
(* fixing the PrettyPrint per WH's request -- UG -- June 27, 2024*)
(* commenting out the old version of PrettyPrint *)
(* start taken out by UG *)
(* 
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
*)
(* end taken out by UG *)

(* now the new version of Pretty Print -- UG -- June 27, 2024 *)
PrettyPrint = 
  Print[
    CleanUpFunction[# /. 
      { DefiningEquation`Operator`Times -> Dot,
        operatorD[0] -> "I",
        operatorD[n_] :> 
          \!\(D\_(First[RecursionOperator`UserVariables])\^ToString[n]\)
      }
    ] /.
    If[$VersionNumber < 7.0,
      { Derivative[n__][u_][x__] :>
        SequenceForm[u, 
          Subscript[
            SequenceForm @@ 
              Flatten[Table[#[[1]], {#[[2]]}] & /@ Transpose[{{x}, {n}}] ]
          ]
        ],
      u_[Sequence @@ CleanUpFunction[RecursionOperator`UserVariables]] :> u 
      },
      (* $VersionNumber >= 7.0 *)
      {
      Derivative[n__][u_][x__] :>
        Subscript[u,
          Row @@ {Flatten[
          Table[#[[1]], {#[[2]]}] & /@ Transpose[{{x}, {n}}]]}],
      u_[Sequence @@ CleanUpFunction[RecursionOperator`UserVariables]] :> u 
      }
    ]
  ]&;
(* end of PrettyPrint in line above *)

(* WH Aug 24, 2024 *)
DefiningEquation`TeXPrint[expression_, opts__:{TeXPrint->False}] :=
  Module[{ output, operator`Times`Converter,
           x = ToString[ First[ RecursionOperator`UserVariables ] ]           
         },
   If[TeXPrint /. opts,
      operator`Times`Converter = 
        ( ( ToString /@ {##} ) /. List -> StringJoin )&;
(* WH Aug 24, 2024 adding a "" before ToString[ u ] in 6th line below *)
      output = 
        InputForm[ expression ] /. 
          { operatorD[0] :> "I",
            operatorD[i_] :> " D_" <> x <> "^" <> ToString[{i}],
            Derivative[n__][u_][x__] :> 
              ( "" ToString[ u ] <> "_" <> 
                  StringReplace[
                    ToString[
                      Flatten[
                        Transpose[{{n}, {x}}] /. 
                          { {0, _} :> Sequence[], {1, a_} :> a }
                      ]
                    ],
                    "," -> ""
                  ]
              ),
            u_[Sequence @@ RecursionOperator`UserVariables] :> ToString[u]
          };
      output = 
        output /. { Rational[a_, b_] :> 
                    "\frac{" <> ToString[a] <> "}{" <> ToString[b] <> "}",
                    Power[a_, b_] :> 
                      If[Negative[b],
                        "\frac{1}{" <> ToString[a] <> "^{" <> 
                          ToString[-b] <> "}}",
                        ToString[a] <> "^{" <> ToString[b] <> "}"
                      ]
                  };
      output = output /. 
        DefiningEquation`Operator`Times -> operator`Times`Converter;
      output = 
        StringReplace[
          ToString[ output ],
          { "recursionOperator`Private`" -> "",
            "\"" -> ""
          }
        ];
      (* WH Aug 24, 2024 added the line below *)
      (* TO DO TO FIX because this would only work for alpha and beta for now *)
      (* did not work taken out again! *)
      (* 
      output = output /. {alpha -> ToString[alpha]"", 
                            beta -> ToString[alpha]""}; 
      *)
      (* 
      output = output /. {"alpha" -> StringInsert[ToString[alpha],"xxx ",-1], 
                         "beta" -> StringInsert[ToString[beta],"xxx ",-1]}; 
      *)
      (* end of addition *)     
      Print[output]
    ] (* end if *)
  ]; (* end of module DefiningEquation`TeXPrint *)

End[] (* Ends `Private` *)

SetAttributes[RecursionOperator, ReadProtected];
SetAttributes[DefiningEquation, ReadProtected];

EndPackage[]

Print["Recursion Operator Package of September 20, 2007 loaded successfully."];
Print["Last updated on August 24, 2024 by Willy Hereman and Unal Goktas."];

(* ***************************** end of all ******************************** *)

