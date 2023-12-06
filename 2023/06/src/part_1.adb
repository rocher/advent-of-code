-------------------------------------------------------------------------------
--
--  AOC202306 - Advent of Code 2023 - Day 6 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Natural         := 1;
   Filename : constant String := "input.txt";

   type Race_Record is record
      Time     : Natural;
      Distance : Natural;
   end record;

   Races : array (1 .. 4) of Race_Record;

begin
   Input.Open (In_File, Filename);
   for Line_Number in 1 .. 2 loop
      declare
         Line : constant String := Input.Get_Line;
         Last : Natural         := 10;
      begin
         for Race of Races loop
            if Line_Number = 1 then
               Ada.Integer_Text_IO.Get
                 (Line (Last + 1 .. Line'Last), Race.Time, Last);
            else
               Ada.Integer_Text_IO.Get
                 (Line (Last + 1 .. Line'Last), Race.Distance, Last);
            end if;
         end loop;
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   for Race of Races loop
      Count      : Natural := (if Race.Time mod 2 = 0 then 1 else 2);
      Press_Time : Natural := (Race.Time / 2) - 1;
      loop
         Count := @ + 2;
         Press_Time := @ - 1;
         exit when (Race.Time - Press_Time) * Press_Time < Race.Distance;
      end loop;
      Answer := @ * Count;
   end loop;

   Put_Line (f"Answer: {Answer}");
end Part_1;
