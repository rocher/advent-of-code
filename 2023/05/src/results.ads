-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Constants; use Constants;

package Results is

   type Results_Array is array (1 .. Seeds_Intervals) of Long_Integer;
   type Results_Access is access all Results_Array;

end Results;
