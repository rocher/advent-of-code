-------------------------------------------------------------------------------
--
--  AOC202502 - Advent of Code 2025 - Day 2 - Part 1
--  Copyright (c) 2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

begin
   Input.Open (In_File, Filename);
   Line : constant String := Input.Get_Line;
   Input.Close;

   Low : array (1 .. 2) of Integer := [others => 0];
   High : array (1 .. 2) of Integer := [others => 0];

   Low (1) := Line'First;
   High (1) := Low (1);

   while High (2) < Line'Last loop
      while Line (High (1)) /= '-' loop
         High (1) := High (1) + 1;
      end loop;

      First : constant Natural :=
        Natural'Value (Line (Low (1) .. High (1) - 1));
      Low_Length : Natural := High (1) - Low (1) + 1;
      Low (2) := High (1) + 1;
      High (2) := Low (1);

      while Line (High (2)) /= ',' loop
         High (2) := High (2) + 1;
      end loop;

      Last : constant Natural :=
        Natural'Value (Line (Low (2) .. High (2) - 1));
      High_Length : Natural := High (2) - Low (2) + 1;
      Low (1) := High (2) + 1;
      High (1) := Low (1);

      --  only when one of the lengths is even...
      if Low_Length mod 2 = 0 or else High_Length mod 2 = 0 then
         Length : constant Natural :=
           (if Low_Length mod 2 = 0 then Low_Length else High_Length);
         Value : array (1 .. Length) of Natural := [others => 0];
         if Low_Length < High_Length then
            Value (2 .. Length) := Line (Low - Low_Length .. Low - 2);
         end if;
      end if;

   end loop;

   Put_Line (f"Answer: {Answer}");
end Part_1;
