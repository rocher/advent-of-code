-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Almanac_Maps;                       use Almanac_Maps;

package Containers.Almanac_Maps is

   package Almanac_Maps_Lists is new Doubly_Linked_Lists
     (Element_Type => Almanac_Map);

   subtype Almanac_Maps_List is Almanac_Maps_Lists.List;

   function Get_Destination
     (List : Almanac_Maps_List; Source : Long_Integer) return Long_Integer;

end Containers.Almanac_Maps;
