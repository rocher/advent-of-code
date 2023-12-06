-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Constants;               use Constants;
with Containers.Almanac_Maps; use Containers.Almanac_Maps;

package Almanacs is

   type Almanacs_Array is array (1 .. Number_Of_Maps) of Almanac_Maps_List;
   type Almanac_Access is access all Almanacs_Array;

end Almanacs;
