-------------------------------------------------------------------------------
--
--  AOC202304 - Advent of Code 2023 - Day 4 - Part 2
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Part_2 is
   Input    : File_Type;
   Answer   : Natural         := 0;
   Filename : constant String := "input.txt";

   Copies : array (0 .. 10) of Natural := [0 .. 10 => 0];
begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line            : constant String           := Input.Get_Line;
         Card_Numbers    : constant String           := Line (42 .. 116);
         Winning_Numbers : constant String (1 .. 30) := Line (10 .. 39);
         Matches         : Natural                   := 0;
      begin
         Copies (0) := @ + 1;  --  instances = copies won + current card
         Answer     := @ + Copies (0);

         --  count matching numbers
         for I in 1 .. 10 loop
            J : constant Natural := ((I - 1) * 3) + 1;
            if Index (Card_Numbers, Winning_Numbers (J .. J + 2)) > 0 then
               Matches := Matches + 1;
            end if;
         end loop;

         --  win scratchcards below the winning card
         if Matches > 0 then
            for I in 1 .. Matches loop
               Copies (I) := @ + Copies (0);
            end loop;
         end if;
      end;

      exit when Input.End_Of_File;

      --  prepare for next card
      Copies (0 .. 9) := Copies (1 .. 10);
      Copies (10)     := 0;
   end loop;
   Input.Close;

   Put_Line (f"Answer: {Answer}");
end Part_2;
