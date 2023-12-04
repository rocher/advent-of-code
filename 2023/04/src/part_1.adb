-------------------------------------------------------------------------------
--
--  AOC202304 - Advent of Code 2023 - Day 4 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Natural         := 0;
   Filename : constant String := "input.txt";

begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line            : constant String           := Input.Get_Line;
         Card_Numbers    : constant String           := Line (42 .. 116);
         Winning_Numbers : constant String (1 .. 30) := Line (10 .. 39);
         Matches         : Natural                   := 0;
      begin
         --  count matching numbers
         for I in 1 .. 10 loop
            J : constant Natural := ((I - 1) * 3) + 1;
            if Index (Card_Numbers, Winning_Numbers (J .. J + 2)) > 0 then
               Matches := Matches + 1;
            end if;
         end loop;

         --  double points according to matching numbers
         if Matches > 0 then
            Answer := @ + 2**(Matches - 1);
         end if;
      end;

      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Put_Line (f"Answer: {Answer}");
end Part_1;
