-------------------------------------------------------------------------------
--
--  AOC202501 - Advent of Code 2025 - Day 1 - Part 2
--  Copyright (c) 2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_2 is
   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

   subtype Dial_Type is Integer range 0 .. 99;

   Dial_Start : Dial_Type := 50;
   Dial_End   : Dial_Type;
begin
   Input.Open (In_File, Filename);
   while not Input.End_Of_Line loop

      Line : constant String := Input.Get_Line;
      Direction : constant Character := Line (1);
      Steps : constant Natural := Integer'Value (Line (2 .. Line'Last));
      Loops : constant Natural := Steps / 100;


      if Direction = 'L' then
         Dial_End := (Dial_Start - Steps) mod 100;
         if Dial_End > Dial_Start
             and then Dial_Start /= 0
             and then Dial_End /= 0
         then
            Answer := Answer + 1;
         end if;
      elsif Direction = 'R' then
         Dial_End := (Dial_Start + Steps) mod 100;
         if Dial_End < Dial_Start
             and then Dial_Start /= 0
             and then Dial_End /= 0
         then
            Answer := Answer + 1;
         end if;
      end if;

      Answer := Answer + Loops;
      Answer := Answer + (if Dial_End = 0 then 1 else 0);
      Dial_Start := Dial_End;

   end loop;
   Input.Close;

   Put_Line (f"Answer: {Answer}");
end Part_2;
