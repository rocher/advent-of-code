-------------------------------------------------------------------------------
--
--  AOC202307 - Advent of Code 2023 - Day 7 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;

package Camel_Cards is

   type Cards is ('2', '3', '4', '5', '6', '7', '8', '9', T, J, Q, K, A);

   type Strengths is
     (High_Card, One_Pair, Two_Pair, Three_Of_A_Kind, Full_House,
      Four_Of_A_Kind, Five_Of_A_Kind);

   type Cards_Counter is array (Cards) of Natural;

   type Hands is array (1 .. 5) of Cards;

   type Plays is record
      Hand : Hands;
      Bid  : Natural;
   end record;

   function Strength (Hand : Hands) return Strengths;
   function "<" (Left, Right : Plays) return Boolean;

   package Hands_Package is new Doubly_Linked_Lists (Plays);
   subtype Hands_Lists is Hands_Package.List;

   package Hands_Sorting is new Hands_Package.Generic_Sorting;

end Camel_Cards;
