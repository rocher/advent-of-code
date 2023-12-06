-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Almanac_Maps is

   type Almanac_Map is record
      Source      : Long_Integer;
      Destination : Long_Integer;
      Length      : Long_Integer;
   end record;

   function Get_Destination
     (Map : Almanac_Map; Source : Long_Integer; Has_Map : in out Boolean)
      return Long_Integer;

end Almanac_Maps;
