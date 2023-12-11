-------------------------------------------------------------------------------
--
--  AOC202311 - Advent of Code 2023 - Day 11 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Part_1 is
   subtype Image_Range is Natural range 1 .. 140;
   subtype Universe_Range is
     Natural range Image_Range'First .. 2 * Image_Range'Last;

   type Images is array (Image_Range, Image_Range) of Character;
   type Universes is array (Universe_Range, Universe_Range) of Character;

   type Position is record
      X, Y : Natural;
   end record;

   package Galaxy_Package is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Position);
   subtype Galaxy_Vector is Galaxy_Package.Vector;
   use all type Galaxy_Package.Cursor;

   Space  : constant Character := '.';
   Galaxy : constant Character := '#';

   Input    : File_Type;
   Answer   : Natural         := 0;
   Filename : constant String := "input.txt";
   Line     : String (Image_Range);

   Image            : Images        := [others => [others => Space]];
   Universe         : Universes     := [others => [others => Space]];
   Universe_Range_X : Natural       := Image_Range'Last;
   Universe_Range_Y : Natural       := Image_Range'Last;
   Galaxies         : Galaxy_Vector := Galaxy_Package.Empty_Vector;

begin
   Input.Open (In_File, Filename);
   for Y in Image_Range loop
      Line := Input.Get_Line;
      for X in Image_Range loop
         Image (X, Y) := Line (X);
      end loop;
   end loop;
   Input.Close;

   --  expand universe
   declare
      Δx, Δy : Natural := 0;
   begin
      for Y in Image_Range loop
         if (for all X in Image_Range => Image (X, Y) = Space) then
            Δy := @ + 1;
         end if;
         Δx := 0;
         for X in Image_Range loop
            if (for all Y in Image_Range => Image (X, Y) = Space) then
               Δx := @ + 1;
            end if;
            Universe (X + Δx, Y + Δy) := Image (X, Y);
         end loop;
      end loop;
      Universe_Range_Y := @ + Δy;
      Universe_Range_X := @ + Δx;
   end;

   --  localize galaxies
   for Y in Universe_Range'First .. Universe_Range_Y loop
      for X in Universe_Range'First .. Universe_Range_X loop
         if Universe (X, Y) = Galaxy then
            Galaxies.Append ((X, Y), 1);
         end if;
      end loop;
   end loop;

   --  compute distances
   declare
      G1, G2 : Galaxy_Package.Cursor;
      P1, P2 : Position;
   begin
      G1 := Galaxies.First;
      loop
         P1 := G1.Element;
         G2 := G1.Next;
         loop
            P2     := G2.Element;
            Answer := @ + abs (P1.X - P2.X) + abs (P1.Y - P2.Y);

            G2 := G2.Next;
            exit when not G2.Has_Element;
         end loop;

         G1 := G1.Next;
         exit when G1 = Galaxies.Last;
      end loop;
   end;

   Put_Line (f"Answer: {Answer}");
end Part_1;
