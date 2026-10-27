-------------------------------------------------------------------------------
--
--  AOC202403 - Advent of Code 2024 - Day 3 - Part 2
--  Copyright (c) 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_2 is
   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

   X_MAS : constant String := "MAS";

   type Line_Range is range 1 .. 140;
   Word_Search : array (Line_Range, Line_Range) of Character;

   First : constant Line_Range := Line_Range'First;
   Last  : constant Line_Range := Line_Range'Last;

   function Count_X_MAS (X, Y : Line_Range) return Natural
   with
     Pre => (First < X and then X < Last and then First < Y and then Y < Last)
   is
      Word : String (1 .. 3) := "*A*";
   begin
      return Count : Natural := 0 do
         Word (1) := Word_Search (X - 1, Y - 1);
         Word (3) := Word_Search (X + 1, Y + 1);
         Count := @ + (if Word = X_MAS then 1 else 0);

         Word (1) := Word_Search (X + 1, Y + 1);
         Word (3) := Word_Search (X - 1, Y - 1);
         Count := @ + (if Word = X_MAS then 1 else 0);

         Word (1) := Word_Search (X - 1, Y + 1);
         Word (3) := Word_Search (X + 1, Y - 1);
         Count := @ + (if Word = X_MAS then 1 else 0);

         Word (1) := Word_Search (X + 1, Y - 1);
         Word (3) := Word_Search (X - 1, Y + 1);
         Count := @ + (if Word = X_MAS then 1 else 0);

         if Count >= 1 then
            Count := Count - 1;
         end if;
      end return;
   end Count_X_MAS;

begin
   Input.Open (In_File, Filename);
   for Y in Line_Range loop
      declare
         X    : Line_Range := First;
         Line : constant String := Input.Get_Line;
      begin
         for C of Line loop
            Word_Search (X, Y) := C;
            X := X + (if X < Last then 1 else 0);
         end loop;
      end;
   end loop;
   Input.Close;

   for X in First + 1 .. Last - 1 loop
      for Y in First + 1 .. Last - 1 loop
         if Word_Search (X, Y) = 'A' then
            Answer := Answer + Count_X_MAS (X, Y);
         end if;
      end loop;
   end loop;

   Put_Line (f"Answer: {Answer}");
end Part_2;
