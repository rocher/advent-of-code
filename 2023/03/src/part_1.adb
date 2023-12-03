-------------------------------------------------------------------------------
--
--  AOC202303 - Advent of Code 2023 - Day 3 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Natural         := 0;
   Filename : constant String := "input.txt";

   subtype Schematic_Support is Natural range 0 .. 141;
   subtype Schematic_Range is Natural range 1 .. 140;

   type Schematic_Type is
     array (Schematic_Support, Schematic_Support) of Character;

   Schematic : Schematic_Type;

   function Σ_Part_Numbers (X, Y : Schematic_Range) return Natural is
   begin
      return Σ_PN : Natural := 0 do
         if Schematic (X, Y) in '.' | '0' .. '9' then
            return;
         end if;
         for Δx in -1 .. 1 loop
            for Δy in -1 .. 1 loop
               if Schematic (X + Δx, Y + Δy) in '0' .. '9' then
                  declare
                     PN          : String (1 .. 3) := "000";
                     First, Last : Natural         := Y + Δy;
                  begin
                     while Schematic (X + Δx, First - 1) in '0' .. '9' loop
                        First := @ - 1;
                     end loop;
                     while Schematic (X + Δx, Last + 1) in '0' .. '9' loop
                        Last := @ + 1;
                     end loop;
                     for I in reverse First .. Last loop
                        PN (3 - (Last - I))   := Schematic (X + Δx, I);
                        Schematic (X + Δx, I) := '.';
                     end loop;
                     Σ_PN := @ + Natural'Value (PN);
                  end;
               end if;
            end loop;
         end loop;
      end return;
   end Σ_Part_Numbers;

begin
   Input.Open (In_File, Filename);
   for X in Schematic_Range loop
      declare
         Line : constant String := Input.Get_Line;
      begin
         for Y in Schematic_Range loop
            Schematic (X, Y) := Line (Y);
         end loop;
      end;
   end loop;
   Input.Close;

   for X in Schematic_Range loop
      for Y in Schematic_Range loop
         Answer := @ + Σ_Part_Numbers (X, Y);
      end loop;
   end loop;

   Put_Line ("Answer:" & Answer'Image);
end Part_1;
