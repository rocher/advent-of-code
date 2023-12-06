-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Almanacs; use Almanacs;
with Results;  use Results;

package Workers_Tasks is

   task type Worker_Task is
      entry Initialize
        (Worker_Index : Natural; Shared_Almanac : not null Almanac_Access;
         Local_Start, Local_Length : Long_Integer;
         Shared_Results            : not null Results_Access);
   end Worker_Task;

   type Worker_Access is access Worker_Task;

end Workers_Tasks;
