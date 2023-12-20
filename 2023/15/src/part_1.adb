-------------------------------------------------------------------------------
--
--  AOC202315 - Advent of Code 2023 - Day 15 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Natural         := 0;
   Filename : constant String := "input.txt";

   function Hash (S : String) return Natural is
   begin
      return Result : Natural := 0 do
         for C of S loop
            Result := @ + Character'Pos (C);
            Result := (@ * 17) mod 256;
         end loop;
      end return;
   end Hash;
begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line  : constant String := Input.Get_Line;
         Start : Natural         := Line'First;
         Index : Natural;
      begin
         loop
            Index := Ada.Strings.Fixed.Index (Line (Start .. Line'Last), ",");
            exit when Index = 0;
            Answer := @ + Hash (Line (Start .. Index - 1));
            Start  := Index + 1;
         end loop;
         Answer := @ + Hash (Line (Start .. Line'Last));
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Put_Line (f"Answer: {Answer}");
end Part_1;
