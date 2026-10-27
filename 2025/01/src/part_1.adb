-------------------------------------------------------------------------------
--
--  AOC202501 - Advent of Code 2025 - Day 1 - Part 1
--  Copyright (c) 2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

   subtype Dial_Type is Integer range 0 .. 99;
   Dial : Dial_Type := 50;
begin
   Input.Open (In_File, Filename);
   while not Input.End_Of_Line loop

      Line : constant String := Input.Get_Line;
      Direction : constant Character := Line (1);
      Steps : constant Natural := Integer'Value (Line (2 .. Line'Last));

      if Direction = 'L' then
         Dial := (Dial - Steps) mod 100;
      elsif Direction = 'R' then
         Dial := (Dial + Steps) mod 100;
      end if;
      Answer := @ + (if Dial = 0 then 1 else 0);

   end loop;
   Input.Close;

   Put_Line (f"Answer: {Answer}");
end Part_1;
