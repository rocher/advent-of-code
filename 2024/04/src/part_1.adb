-------------------------------------------------------------------------------
--
--  AOC202403 - Advent of Code 2024 - Day 3 - Part 1
--  Copyright (c) 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

   XMAS : constant String := "XMAS";

   type Line_Range is range 1 .. 140;
   Word_Search : array (Line_Range, Line_Range) of Character;

   First : constant Line_Range := Line_Range'First;
   Last  : constant Line_Range := Line_Range'Last;

   function At_Limit (Z : Line_Range; Δz : Integer) return Boolean is
   begin
      return Limit : Boolean := False do
         case Δz is
            when -1 =>
               Limit := (Z = First);

            when 1 =>
               Limit := (Z = Last);

            when others =>
               null;
         end case;
      end return;
   end At_Limit;

   function Count_XMAS
     (Start_X, Start_Y : Line_Range; Δx, Δy : Integer) return Natural
   is
      X    : Line_Range := Start_X;
      Y    : Line_Range := Start_Y;
      Word : String (1 .. 4) := "X***";

      End_Loop : Boolean := False;
   begin
      return Count : Natural := 0 do
         for P in 2 .. XMAS'Length loop
            if Δy = 0 then
               --  necessarily Δx = { -1, 1 }
               End_Loop := At_Limit (X, Δx);
            elsif Δx = 0 then
               --  necessarily Δy = { -1, 1 }
               End_Loop := At_Limit (Y, Δy);
            else
               --  Δx = { -1, 1 } and Δy = { -1, 1 }
               End_Loop := At_Limit (X, Δx) or else At_Limit (Y, Δy);
            end if;
            exit when End_Loop;

            X := Line_Range (Integer (X) + Δx);
            Y := Line_Range (Integer (Y) + Δy);
            Word (P) := Word_Search (X, Y);
         end loop;

         Count := (if Word = XMAS then 1 else 0);
      end return;
   end Count_XMAS;

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

   for X in Line_Range loop
      for Y in Line_Range loop
         if Word_Search (X, Y) = 'X' then
            Answer :=
              @ + Count_XMAS (X, Y, 1, 0) + Count_XMAS (X, Y, -1, 0)
              + Count_XMAS (X, Y, 0, -1)
              + Count_XMAS (X, Y, 0, 1)
              + Count_XMAS (X, Y, -1, -1)
              + Count_XMAS (X, Y, 1, 1)
              + Count_XMAS (X, Y, -1, 1)
              + Count_XMAS (X, Y, 1, -1);
         end if;
      end loop;
   end loop;

   Put_Line (f"Answer: {Answer}");
end Part_1;
