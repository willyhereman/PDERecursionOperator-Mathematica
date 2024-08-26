(* : Description : Sets up Menu driven version of the function. *)
(* : Date : Tuesday, July 16, 2002 *)

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
    Print["17. Kaup-Kupershmidt with parameters"];
    Print["x   Abort"];

    (* Gets the choice from the user. *)
    choice = Input["Enter your choice: "];

    (* Runs the choice entered by the user. *)
    Switch[choice,
      x, Abort[],
      1, DefiningEquation[
           6*u[x,t]*D[u[x,t], x] + D[u[x,t], {x, 3}],
           a*D[#, {x, 2}] + b*u[x,t]*# + 
             c*D[u[x,t], x]*Integrate[#, x]&,
           u[x,t],
           {x,t}
         ],
      2, DefiningEquation[
           3*D[u[x,t], x]^2 + D[u[x,t], {x, 3}],
           a*D[#, {x, 2}] + b*D[u[x,t], x]*# + c*Integrate[#, x]*D[u[x,t], {x, 2}]&,
           u[x,t], 
           {x,t}
         ],
      3, DefiningEquation[
           u[x,t]^2*D[u[x,t], x] + D[u[x,t], {x, 3}],
           c1*D[#, {x, 2}] + c2*u[x,t]*D[#, x] + c3*u[x,t]^2*# 
            + c4*D[u[x,t], x]*# + c5*D[u[x,t], x]*Integrate[u[x,t]*#, x]&,
           u[x,t],
           {x,t}
         ],
      4, DefiningEquation[
           1/3*D[u[x,t], x]^3 + D[u[x,t], {x, 3}],
           a*D[#, {x, 2}] + b*D[u[x,t], x]^2*# + c*D[u[x,t], x]*Integrate[D[u[x,t], {x, 2}]*#, x]&,
           u[x,t],
           {x,t}
         ],
      5, DefiningEquation[
           u[x,t]^2*D[u[x,t], {x, 2}],
           a*u[x,t]*D[#, x] + b*u[x,t]^2*D[u[x,t], {x, 2}]*Integrate[1/u[x,t]^2*#, x]&, 
         (*  a * D[#, {x, 2}] + b * u[x,t] * D[#, x] + c * u[x,t]^2 * # + d * D[u[x,t], x] * #
            + e * u[x,t]^2 * D[u[x,t], {x, 2}] * Integrate[1/u[x,t]^2 * #, x]&, *)
           u[x,t],
           {x,t}
         ],
      6, DefiningEquation[
           u[x,t]*D[u[x,t], x] + D[u[x,t], {x, 2}],
           a*D[#, x] + b*u[x,t]*# + c*D[u[x,t], x]*Integrate[#, x]&,
           u[x,t],
           {x,t}
         ],
      7, DefiningEquation[
           D[u[x,t], x]^2 + D[u[x,t], {x, 2}],
           a*D[#, x] + b*D[u[x,t], x]*#&,
           u[x,t],
           {x,t}
         ],
      8, DefiningEquation[
           D[u[x,t], x]^2 + D[u[x,t], {x, 2}],
           a*t*D[#, x] + b*t*D[u[x,t], x]*# + c*1/2*x*#&,
           u[x,t],
           {x,t}
         ],
      9, DefiningEquation[
           20*u[x,t]^2*D[u[x,t], x] 
            + 25*D[u[x,t], x]*D[u[x,t], {x, 2}] 
            + 10*u[x,t]*D[u[x,t], {x, 3}] 
            + D[u[x,t], {x, 5}],
           a*D[#, {x, 6}] 
            + b*u[x,t]*D[#, {x, 4}] 
            + c*D[u[x,t],x]*D[#, {x, 3}] 
            + d*u[x,t]^2*D[#, {x, 2}] 
            + e*D[u[x,t], {x, 2}]*D[#, {x, 2}] 
            + f*u[x,t]*D[u[x,t],x]*D[#, x] 
            + g*D[u[x,t], {x,3}]*D[#, x] 
            + h*u[x,t]^3*#
            + i*u[x,t]*D[u[x,t], {x, 2}]*#
            + j*D[u[x,t],x]^2*#
            + k*D[u[x,t],{x,4}]*#
            + l*D[u[x,t], x]*Integrate[4*u[x,t]^2*# + D[u[x,t], {x,2}]*#, x]
            + m*(20*u[x,t]^2*D[u[x,t], x] + 25*D[u[x,t], x]*D[u[x,t], {x, 2}] 
              + 10*u[x,t]*D[u[x,t], {x, 3}] + D[u[x,t], {x, 5}])*Integrate[#, x]&,
(* Incorrect form from Unal's Thesis
           a*D[#, {x, 6}] + u[x,t]*D[#, {x, 4}] 
            + b*D[u[x,t]*D[#, {x, 3}], x]
            + c*D[u[x,t]*D[#, {x, 2}], {x, 2}] 
            + d*D[u[x,t]*D[#, x], {x, 3}]
            + e*D[u[x,t]*#, {x, 4}] - 3*u[x,t]^2*D[#, {x, 2}] 
            - f*u[x,t]*D[u[x,t]*D[#, x], x] 
            + g*u[x,t]*D[u[x,t]*#, {x, 2}]
            - h*D[u[x,t]*D[u[x,t]*#, x], x] 
            + i*D[u[x,t]*Integrate[#, x], {x, 5}]
            - j*u[x,t]*D[u[x,t]*Integrate[#, x], {x, 3}]
            + k*D[u[x,t]*D[u[x,t]*Integrate[#, x], {x, 2}], x]
            + l*u[x,t]^2*D[u[x,t]*Integrate[#, x], x]
            + m*u[x,t]*D[u[x,t]^2*Integrate[#, x], x]
            + n*D[u[x,t]*Integrate[4*u[x,t]^2*# 
            + o*D[u[x,t], x]*D[#, x], x], x]&,
*)
           u[x,t],
           {x,t}
         ],
      10, DefiningEquation[
           5*u[x,t]^2*D[u[x,t], x] + 5*u[x,t]*D[u[x,t], {x, 3}] + 
             5*D[u[x,t], x]*D[u[x,t], {x, 2}] + D[u[x,t], {x, 5}],
           a*D[#, {x, 6}] + 
             b*u[x,t]*D[#, {x, 4}] +              
             c*D[u[x,t], x]*D[#, {x, 3}] + 
             d*u[x,t]^2*D[#, {x, 2}] + 
             e*D[u[x,t], {x, 2}]*D[#, {x,2}] + 
             f*D[u[x,t], {x, 3}]*D[#, x] + 
             g*u[x,t]*D[u[x,t], x]*D[#, x] + 
             h*u[x,t]^3*# + 
             i*u[x,t]*D[u[x,t], {x, 2}]*# + 
             j*D[u[x,t], x]^2*# + 
             k*D[u[x,t], {x, 4}]*# + 
             l*D[u[x,t], x]*Integrate[u[x,t]^2*#, x] +
             m*D[u[x,t], x]*Integrate[2*D[u[x,t], {x,2}]*#, x] + 
             n*u[x,t]^2*D[u[x,t], x]*Integrate[#, x] + 
             o*D[u[x,t], x]*D[u[x,t], {x, 2}]*Integrate[#, x] + 
             p*u[x,t]*D[u[x,t], {x, 3}]*Integrate[#, x] + 
             q*D[u[x,t], {x, 5}]*Integrate[#, x]&,
           u[x,t],
           {x,t}
         ],
      11, DefiningEquation[
            20/3*D[u[x,t], {x, 3}] + 15/2*D[u[x,t], {x,2}]^2 
             + 10*D[u[x,t], x]*D[u[x,t], {x, 3}] + D[u[x,t], {x,5}],
            a*D[#, {x, 6}] 
             + b*u[x,t]*D[#, {x, 5}] 
             + c*D[u[x,t]*D[#, {x, 4}], x]
             + d*D[u[x,t]*D[#, {x, 3}], {x, 2}] 
             + e*D[u[x,t]*D[#, x], {x, 4}]
             + f*D[u[x,t]*#, {x, 5}] 
             + g*u[x,t]^2*D[#, {x, 4}]
             + h*u[x,t]*D[u[x,t]*D[#, {x, 3}], x] 
             + i*u[x,t]*D[u[x,t]*D[#, {x, 2}], {x, 2}]
             + j*u[x,t]*D[u[x,t]*D[#, x], {x, 3}] 
             + k*u[x,t]*D[u[x,t]*#, {x, 4}] 
             + l*D[u[x,t]*D[u[x,t]*D[#, {x, 2}], x], x] 
             + m*D[u[x,t]*D[u[x,t]*D[#, x], {x, 2}], x]
             + n*D[u[x,t]*D[u[x,t]*#, {x, 3}], x]
             + o*D[u[x,t]*D[u[x,t]*#, {x, 2}], {x, 2}]
             + p*u[x,t]^3*D[#, {x, 3}]
             + q*u[x,t]^2*D[u[x,t]*D[#, {x, 2}], x]
             + r*u[x,t]^2*D[u[x,t]*D[#, x], {x, 2}]
             + s*u[x,t]^2*D[u[x,t]*#, {x, 3}]
             + v*u[x,t]*D[u[x,t]*D[u[x,t]*D[#, x], x], x]
             + w*u[x,t]*D[u[x,t]*D[u[x,t]*#, {x, 2}], x]
             + y*D[u[x,t]*D[u[x,t]*D[u[x,t]*#, x], x], x]
             + z*Integrate[32*u[x,t]^3*D[#, x] 
                   + 18*D[u[x,t], {x, 2}]^2*D[#, x]
                   + 36*D[u[x,t], x]*D[u[x,t], {x, 2}]*D[#, {x, 2}]
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
            alpha*u[x,t]^2*D[u[x,t], x] 
              + beta*D[u[x,t], x]*D[u[x,t], {x, 2}] 
              + gamma*u[x,t]*D[u[x,t], {x, 3}] 
              + D[u[x,t], {x, 5}],
            a*D[#, {x, 6}] 
              + b*u[x,t]*D[#, {x, 4}] 
              + c*D[u[x,t],x]*D[#, {x, 3}] 
              + d*u[x,t]^2*D[#, {x, 2}] 
              + e*D[u[x,t], {x, 2}]*D[#, {x, 2}] 
              + f*u[x,t]*D[u[x,t],x]*D[#, x] 
              + g*D[u[x,t], {x,3}]*D[#, x] 
              + h*u[x,t]^3*#
              + i*u[x,t]*D[u[x,t], {x, 2}]*#
              + j*D[u[x,t],x]^2*#
              + k*D[u[x,t],{x,4}]*#
              + l*D[u[x,t], x]*Integrate[4*u[x,t]^2*# + D[u[x,t], {x,2}]*#, x]
              + m*(alpha*u[x,t]^2*D[u[x,t], x] + beta*D[u[x,t], x]*D[u[x,t], {x, 2}] 
              + gamma*u[x,t]*D[u[x,t], {x, 3}] + D[u[x,t], {x, 5}])*Integrate[#, x]&,
            u[x,t],
            {x,t},
            {alpha, beta, gamma}
          ],
      ___, Abort[]
    ]
  ];