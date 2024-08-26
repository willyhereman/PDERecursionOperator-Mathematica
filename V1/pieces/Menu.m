(* : Description : Sets up Menu driven version of the function. *)
(* : Date : Wednesday, July 10, 2002 *)

DefiningEquation[] :=
  Block[{choice},
    (* Prints the choice of cases. *)
    Print["1.  The Korteweg-de Vries"];
    Print["2.  Potential Korteweg-de Vries"];
    Print["3.  Modefied Korteweg-de Vries"];
    Print["4.  Potential Modified Korteweg-de Vries"];
    Print["5.  Diffusion Equation"];
    Print["6.  Burgers' Equation"];
    Print["7.  Potential Burgers' Equation"];
    Print["8.  Potential Burgers' Equation (with t explicitly in R)"];
    Print["9.  Kaup-Kupershmidt Equation"];
    Print["10. Sawada-Kotera Equation"];
    Print["11. Potential Kaup-Kupershmidt*"];
    Print["12. Potential Sawada-Kotera Equation"];
    Print["13. Diffusion System"];
    Print["14. Dispersiveless Long Wave System"];
    Print["15. AKNS"];
    Print["16. MDV"];
    Print["17. Lax"];
    Print["x   Abort"];

    (* Gets the choice from the user. *)
    choice = Input["Enter your choice: "];

    (* Runs the choice entered by the user. *)
    Switch[choice,
      x, Abort[],
      1, DefiningEquation[
           6*u[x,t]*D[u[x,t], x] + D[u[x,t], {x, 3}],
           D[#, {x, 2}] + 4*u[x,t]*# + 
             2*D[u[x,t], x]*Integrate[#, x]&,
           u[x,t],
           {x,t}
         ],
      2, DefiningEquation[
           3*D[u[x,t], x]^2 + D[u[x,t], {x, 3}],
           D[#, {x, 2}] + 4*D[u[x,t], x]*# - 2*Integrate[#, x]*D[u[x,t], {x, 2}]&,
           u[x,t], 
           {x,t}
         ],
      3, DefiningEquation[
           u[x,t]^2*D[u[x,t], x] + D[u[x,t], {x, 3}],
           D[#, {x, 2}] + 2/3*u[x,t]^2*# + 2/3*D[u[x,t], x]*Integrate[u[x,t]*#, x]&,
           u[x,t],
           {x,t}
         ],
      4, DefiningEquation[
           1/3*D[u[x,t], x]^3 + D[u[x,t], {x, 3}],
           D[#, {x, 2}] + 2/3*D[u[x,t], x]^2*# - 2/3*D[u[x,t], x]*Integrate[D[u[x,t], {x, 2}]*#, x]&,
           u[x,t],
           {x,t}
         ],
      5, DefiningEquation[
           u[x,t]^2*D[u[x,t], {x, 2}],
           u[x,t]*D[#, x] + u[x,t]^2*D[u[x,t], {x, 2}]*Integrate[1/u[x,t]^2*#, x]&,
           u[x,t],
           {x,t}
         ],
      6, DefiningEquation[
           u[x,t]*D[u[x,t], x] + D[u[x,t], {x, 2}],
           D[#, x] + 1/2*u[x,t]*# + 1/2*D[u[x,t], x]*Integrate[#, x]&,
           u[x,t],
           {x,t}
         ],
      7, DefiningEquation[
           D[u[x,t], x]^2 + D[u[x,t], {x, 2}],
           D[#, x] + D[u[x,t], x]*#&,
           u[x,t],
           {x,t}
         ],
      8, DefiningEquation[
           D[u[x,t], x]^2 + D[u[x,t], {x, 2}],
           t*D[#, x] + t*D[u[x,t], x]*# + 1/2*x*#&,
           u[x,t],
           {x,t}
         ],
      9, DefiningEquation[
           20*u[x,t]^2*D[u[x,t], x] + 25*D[u[x,t], x]*D[u[x,t], {x, 2}] 
            + 10*u[x,t]*D[u[x,t], {x, 3}] + D[u[x,t], {x, 5}],
(* This is from Sanders paper. *)
           D[#, {x, 6}] 
            + 12*u[x,t]*D[#, {x, 4}] 
            + 36*D[u[x,t],x]*D[#, {x, 3}] 
            + 36*u[x,t]^2*D[#, {x, 2}] 
            + 49*D[u[x,t], {x, 2}]*D[#, {x, 2}] 
            + 120*u[x,t]*D[u[x,t],x]*D[#, x] 
            + 35*D[u[x,t], {x,3}]*D[#, x] 
            + 32*u[x,t]^3*#
            + 82*u[x,t]*D[u[x,t], {x, 2}]*#
            + 69*D[u[x,t],x]^2*#
            + 13*D[u[x,t],{x,4}]*#
            + 2*D[u[x,t], x]*Integrate[4*u[x,t]^2*# + D[u[x,t], {x,2}]*#, x]
            + 2*(20*u[x,t]^2*D[u[x,t], x] + 25*D[u[x,t], x]*D[u[x,t], {x, 2}] 
              + 10*u[x,t]*D[u[x,t], {x, 3}] + D[u[x,t], {x, 5}])*Integrate[#, x]&,
(* This is from Unal's Thesis 
           D[#, {x, 6}] + u[x,t]*D[#, {x, 4}] + D[u[x,t]*D[#, {x, 3}], x]
            + 2*D[u[x,t]*D[#, {x, 2}], {x, 2}] + 3*D[u[x,t]*D[#, x], {x, 3}]
            + 3*D[u[x,t]*#, {x, 4}] - 3*u[x,t]^2*D[#, {x, 2}] 
            - 3*u[x,t]*D[u[x,t]*D[#, x], x] + 51*u[x,t]*D[u[x,t]*#, {x, 2}]
            - 29*D[u[x,t]*D[u[x,t]*#, x], x] + 2*D[u[x,t]*Integrate[#, x], {x, 5}]
            - 30*u[x,t]*D[u[x,t]*Integrate[#, x], {x, 3}]
            + 50*D[u[x,t]*D[u[x,t]*Integrate[#, x], {x, 2}], x]
            + 8*u[x,t]^2*D[u[x,t]*Integrate[#, x], x]
            + 16*u[x,t]*D[u[x,t]^2*Integrate[#, x], x]
            + 2*D[u[x,t]*Integrate[4*u[x,t]^2*# - D[u[x,t], x]*D[#, x], x], x]&,
*)
           u[x,t],
           {x,t}
         ],
      10, DefiningEquation[
           5*u[x,t]^2*D[u[x,t], x] + 5*u[x,t]*D[u[x,t], {x, 3}] + 
             5*D[u[x,t], x]*D[u[x,t], {x, 2}] + D[u[x,t], {x, 5}],
           D[#, {x, 6}] + 
             6*u[x,t]*D[#, {x, 4}] +              
             9*D[u[x,t], x]*D[#, {x, 3}] + 
             9*u[x,t]^2*D[#, {x, 2}] + 
             11*D[u[x,t], {x, 2}]*D[#, {x,2}] + 
             10*D[u[x,t], {x, 3}]*D[#, x] + 
             21*u[x,t]*D[u[x,t], x]*D[#, x] + 
             4*u[x,t]^3*# + 
             16*u[x,t]*D[u[x,t], {x, 2}]*# + 
             6*D[u[x,t], x]^2*# + 
             5*D[u[x,t], {x, 4}]*# + 
             D[u[x,t], x]*Integrate[u[x,t]^2*#, x] +
             D[u[x,t], x]*Integrate[2*D[u[x,t], {x,2}]*#, x] + 
             5*u[x,t]^2*D[u[x,t], x]*Integrate[#, x] + 
             5*D[u[x,t], x]*D[u[x,t], {x, 2}]*Integrate[#, x] + 
             5*u[x,t]*D[u[x,t], {x, 3}]*Integrate[#, x] + 
             D[u[x,t], {x, 5}]*Integrate[#, x]&,
           u[x,t],
           {x,t}
         ],
      11, DefiningEquation[
            20/3*D[u[x,t], {x, 3}] + 15/2*D[u[x,t], {x,2}]^2 
             + 10*D[u[x,t], x]*D[u[x,t], {x, 3}] + D[u[x,t], {x,5}],
            D[#, {x, 6}] 
             - 3*u[x,t]*D[#, {x, 5}] 
             + 2*D[u[x,t]*D[#, {x, 4}], x]
             - D[u[x,t]*D[#, {x, 3}], {x, 2}] 
             + D[u[x,t]*D[#, x], {x, 4}]
             + 2*D[u[x,t]*#, {x, 5}] 
             + 11*u[x,t]^2*D[#, {x, 4}]
             + 2*u[x,t]*D[u[x,t]*D[#, {x, 3}], x] 
             - 45*u[x,t]*D[u[x,t]*D[#, {x, 2}], {x, 2}]
             + 28*u[x,t]*D[u[x,t]*D[#, x], {x, 3}] 
             - 5*u[x,t]*D[u[x,t]*#, {x, 4}] 
             + 32*D[u[x,t]*D[u[x,t]*D[#, {x, 2}], x], x] 
             - 28*D[u[x,t]*D[u[x,t]*D[#, x], {x, 2}], x]
             - 10*D[u[x,t]*D[u[x,t]*#, {x, 3}], x]
             + 15*D[u[x,t]*D[u[x,t]*#, {x, 2}], {x, 2}]
             - 64/3*u[x,t]^3*D[#, {x, 3}]
             + 104/3*u[x,t]^2*D[u[x,t]*D[#, {x, 2}], x]
             - 56/3*u[x,t]^2*D[u[x,t]*D[#, x], {x, 2}]
             + 40*u[x,t]^2*D[u[x,t]*#, {x, 3}]
             + 16/3*u[x,t]*D[u[x,t]*D[u[x,t]*D[#, x], x], x]
             - 160/3*u[x,t]*D[u[x,t]*D[u[x,t]*#, {x, 2}], x]
             + 40/3*D[u[x,t]*D[u[x,t]*D[u[x,t]*#, x], x], x]
             + 1/3*Integrate[32*u[x,t]^3*D[#, x] 
                   - 18*D[u[x,t], {x, 2}]^2*D[#, x]
                   - 36*D[u[x,t], x]*D[u[x,t], {x, 2}]*D[#, {x, 2}]
                   + 3*D[u[x,t], {x, 3}]*D[#, {x, 3}],
                 x
               ]&,
            u[x,t],
            {x,t}
          ],
      12, DefiningEquation[
            5/3*D[u[x,t], x]^3 + 5*D[u[x,t], x]*D[u[x,t], {x, 3}] + D[u[x,t], {x, 5}],
            D[#, {x, 6}]
             + 6*D[u[x,t], x]*D[#, {x, 4}]
             + 3*D[u[x,t], {x, 2}]*D[#, {x, 3}]
             + 8*D[u[x,t], {x, 3}]*D[#, {x, 2}]
             + 9*D[u[x,t], x]^2*D[#, {x, 2}]
             + 2*D[u[x,t], {x, 4}]*D[#, x]
             + 3*D[u[x,t], x]*D[u[x,t], {x, 2}]*D[#, x]
             + 3*D[u[x,t], {x, 5}]*#
             + 13*D[u[x,t], {x, 3}]*D[u[x,t], x]*#
             + 3*D[u[x,t], {x, 2}]^2*#
             + 4*D[u[x,t], x]^3*#
             - 2*D[u[x,t], x]*Integrate[D[u[x,t], {x, 4}]*#
                                + D[u[x,t], {x, 2}]*D[u[x,t], x]*# ]
             - 2*Integrate[D[u[x,t], {x, 6}]*# 
                   + 3*D[u[x,t], {x, 4}]*D[u[x,t], x]*#
                   + 6*D[u[x,t], {x, 2}]*D[u[x,t], {x, 3}]*#
                   + 2*D[u[x,t], x]^2*D[u[x,t], {x, 2}]*#
                 ]&,
            u[x,t],
            {x,t}
          ],
      13, DefiningEquation[
           { D[u[x,t], {x, 2}] + v[x,t]^2, 
             D[v[x,t], {x, 2}]
           },
           { { D[#, x]&, v[x,t]*Integrate[#, x]& },
             { 0 &, D[#, x]& }
           },
           {u[x,t], v[x,t]},
           {x,t}
         ],
      14, DefiningEquation[
           { u[x,t]*D[v[x,t], x] + D[u[x,t], x]*v[x,t],
             D[u[x,t], x] + v[x,t]*D[v[x,t], x]
           },
           { { v[x,t]*# &, 2*u[x,t]*# + D[u[x,t], x]*Integrate[#, x]&},
             { 2*# &, v[x,t]*# + D[v[x,t], x]*Integrate[#, x]& }
           },
           {u[x,t], v[x,t]},
           {x,t}
         ],
      15, DefiningEquation[
            { 2*u[x,t]^2*v[x,t] - D[u[x,t], {x, 2}],
              -2*u[x,t]*v[x,t]^2 + D[v[x,t], {x, 2}]
            },
            { { -D[#, x] + 2*u[x,t]*Integrate[v[x,t]*#, x]&, 2*u[x,t]*Integrate[u[x,t]*#, x]& },
              { -2*v[x,t]*Integrate[v[x,t]*#, x]&, D[#, x] - 2*v[x,t]*Integrate[u[x,t]*#, x]& }
            },
            {u[x,t], v[x,t]},
            {x,t}
          ],
      16, DefiningEquation[
            {  \[Beta]*D[u[x,t], x] + 3*D[u[x,t], x]*u[x,t]^2 + D[u[x,t], x]*v[x,t]^2 
                + 2*u[x,t]*D[v[x,t], x]*v[x,t] - D[v[x,t], {x, 2}] + \[Gamma]*D[v[x,t], x],
               D[u[x,t], {x, 2}] + \[Theta]*D[u[x,t], x] + 2*D[u[x,t], x]*u[x,t]*v[x,t] 
                + u[x,t]^2*D[v[x,t], x] + \[Delta]*D[v[x,t], x] + 2*D[v[x,t], x]*v[x,t]^2
            },
            { { (\[Beta] - \[Delta] + 2*u[x,t]^2)*# + 2*D[u[x,t], x]*Integrate[u[x,t]*#, x]&,
                -D[#, x] + (\[Theta] + 2*u[x,t]*v[x,t])*# + 2*D[u[x,t], x]*Integrate[v[x,t]*#, x]&
              }, 
              { D[#, x] + (\[Theta] + 2*u[x,t]*v[x,t])*# + 2*D[v[x,t], x]*Integrate[u[x,t]*#, x]&,
                2*v[x,t]^2*# + 2*D[v[x,t], x]*Integrate[v[x,t]*#, x]&
              }
            },
            {u[x,t], v[x,t]},
            {x,t}
          ],
      17, DefiningEquation[
           30*u[x,t]^2*D[u[x,t], x] + 20*D[u[x,t], x]*D[u[x,t], {x, 2}] 
            + 10*u[x,t]*D[u[x,t], {x, 3}] + D[u[x,t], {x, 5}],
           D[#, {x, 2}] + 4*u[x,t]*# + 
             2*D[u[x,t], x]*Integrate[#, x]&,
           u[x,t],
           {x,t}
         ],
      ___, Abort[]
    ]
  ];