-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Almanac_Maps is

   function Get_Destination
     (Map : Almanac_Map; Source : Long_Integer; Has_Map : in out Boolean)
      return Long_Integer
   is
   begin
      return Destination : Long_Integer := Source do
         if Map.Source <= Source and then Source <= Map.Source + Map.Length - 1
         then
            Destination := Map.Destination + Source - Map.Source;
            Has_Map     := True;
         end if;
      end return;
   end Get_Destination;

end Almanac_Maps;
