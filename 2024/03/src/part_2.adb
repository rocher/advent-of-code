-------------------------------------------------------------------------------
--
--  AOC202403 - Advent of Code 2024 - Day 3 - Part 2
--  Copyright (c) 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regpat;

procedure Part_2 is
   package Reg renames GNAT.Regpat;

   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

   Instruction_Matcher : Reg.Pattern_Matcher (256);
   Matches             : Reg.Match_Array (0 .. 2);
   Enabled             : Boolean := True;

begin
   Instruction_Matcher.Compile
     ("do\(\)|don't\(\)|mul\(([0-9]{1,3}),([0-9]{1,3})\)");

   Input.Open (In_File, Filename);
   loop
      declare
         Line  : constant String := Input.Get_Line;
         First : Integer := 1;
      begin
         loop
            First := Instruction_Matcher.Match (Line, First);
            exit when First = 0;

            Instruction_Matcher.Match (Line, Matches, First);
            if Line (Matches (0).First .. Matches (0).Last) = "do()" then
               Enabled := True;
            elsif Line (Matches (0).First .. Matches (0).Last) = "don't()" then
               Enabled := False;
            elsif Enabled then
               Answer :=
                 @
                 + (Natural'Value
                      (Line (Matches (1).First .. Matches (1).Last))
                    * Natural'Value
                        (Line (Matches (2).First .. Matches (2).Last)));
            end if;
            First := First + 1;
         end loop;
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Put_Line (f"Answer: {Answer}");
end Part_2;
