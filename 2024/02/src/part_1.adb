-------------------------------------------------------------------------------
--
--  AOC202401 - Advent of Code 2024 - Day 1 - Part 1
--  Copyright (c) 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_1 is
   package Natural_IO is new Integer_IO (Natural);

   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

   type Level_Trend is (decrement, increment);

begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line : constant String := Input.Get_Line;
         Last : Positive;

         Previous_Level, Current_Level : Natural;
         Previous_Trend, Current_Trend : Level_Trend;

         Is_Safe : Boolean := True;
      begin
         Natural_IO.Get (Line (1 .. Line'Last), Previous_Level, Last);
         Natural_IO.Get (Line (Last + 1 .. Line'Last), Current_Level, Last);

         Current_Trend :=
           (if Previous_Level > Current_Level then decrement else increment);

         Is_Safe :=
           abs (Previous_Level - Current_Level) >= 1
           and then abs (Previous_Level - Current_Level) <= 3;
         loop
            exit when not Is_Safe or else Last >= Line'Last;

            Previous_Level := Current_Level;
            Previous_Trend := Current_Trend;
            Natural_IO.Get (Line (Last + 1 .. Line'Last), Current_Level, Last);

            Current_Trend :=
              (if Previous_Level > Current_Level then decrement
               else increment);

            Is_Safe :=
              Current_Trend = Previous_Trend
              and then abs (Previous_Level - Current_Level) >= 1
              and then abs (Previous_Level - Current_Level) <= 3;
         end loop;

         if Is_Safe then
            Answer := @ + 1;
         end if;
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Put_Line (f"Answer: {Answer}");
end Part_1;
