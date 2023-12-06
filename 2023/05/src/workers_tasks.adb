-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Containers.Almanac_Maps; use Containers.Almanac_Maps;

package body Workers_Tasks is

   task body Worker_Task is
      Index         : Natural;
      Almanac       : Almanac_Access;
      Start, Length : Long_Integer;
      Results       : Results_Access;
      Destination   : Long_Integer;
      Minimum       : Long_Integer := Long_Integer'Last;
   begin

      select
         accept Initialize (Worker_Index : Natural;
           Shared_Almanac : not null Almanac_Access;
           Local_Start, Local_Length : Long_Integer;
           Shared_Results : not null Results_Access)
         do
            Index   := Worker_Index;
            Almanac := Shared_Almanac;
            Start   := Local_Start;
            Length  := Local_Length;
            Results := Shared_Results;
         end Initialize;
      or
         terminate;
      end select;

      for I in Start .. Start + Length loop
         --  Destination = seed
         Destination := I;
         for I in Almanac'Range loop
            Destination := Get_Destination (Almanac (I), Destination);
         end loop;
         --  Destination = location
         Minimum := Long_Integer'Min (@, Destination);
      end loop;
      Results (Index) := Minimum;

   end Worker_Task;

end Workers_Tasks;
