-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;

package Containers.Seeds is

   package Seeds_Lists is new Doubly_Linked_Lists
     (Element_Type => Long_Integer);

   subtype Seeds_List is Seeds_Lists.List;

end Containers.Seeds;
