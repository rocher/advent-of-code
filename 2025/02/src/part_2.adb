-------------------------------------------------------------------------------
--
--  AOC202502 - Advent of Code 2025 - Day 2 - Part 2
--  Copyright (c) 2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_2 is
   Input    : File_Type;
   Answer   : Natural         := 0;
   Filename : constant String := "input.txt";

begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line : constant String := Input.Get_Line;
      begin
         null;
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Put_Line (f"Answer: {Answer}");
end Part_2;
