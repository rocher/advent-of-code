-------------------------------------------------------------------------------
--
--  AOC202311 - Advent of Code 2023 - Day 11 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Part_2 is
   subtype Image_Range is Natural range 1 .. 140;

   type Images is array (Image_Range, Image_Range) of Character;

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
   Answer   : Long_Integer    := 0;
   Filename : constant String := "input.txt";
   Line     : String (Image_Range);

   Image    : Images        := [others => [others => Space]];
   Galaxies : Galaxy_Vector := Galaxy_Package.Empty_Vector;

begin
   Input.Open (In_File, Filename);
   for Y in Image_Range loop
      Line := Input.Get_Line;
      for X in Image_Range loop
         Image (X, Y) := Line (X);
      end loop;
   end loop;
   Input.Close;

   --  expand universe: mark with 'M' the header of all empty columns/rows
   for Y in Image_Range loop
      if (for all X in Image_Range => Image (X, Y) = Space) then
         Image (1, Y) := 'M';
      end if;
   end loop;
   for X in Image_Range loop
      if (for all Y in Image_Range => Image (X, Y) in Space | 'M') then
         Image (X, 1) := 'M';
      end if;
   end loop;

   --  localize galaxies
   for Y in Image_Range loop
      for X in Image_Range loop
         if Image (X, Y) = Galaxy then
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
            Answer := @ + Long_Integer (abs (P1.X - P2.X) + abs (P1.Y - P2.Y));

            for Y in Natural'Min (P1.Y, P2.Y) .. Natural'Max (P1.Y, P2.Y) loop
               if Image (1, Y) = 'M' then
                  Answer := @ + 999_999; --  1_000_000 - the column itself
               end if;
            end loop;

            for X in Natural'Min (P1.X, P2.X) .. Natural'Max (P1.X, P2.X) loop
               if Image (X, 1) = 'M' then
                  Answer := @ + 999_999; --  1_000_000 - the row itself
               end if;
            end loop;

            G2 := G2.Next;
            exit when not G2.Has_Element;
         end loop;

         exit when G1.Next = Galaxies.Last;
         G1 := G1.Next;
      end loop;
   end;

   Put_Line (f"Answer: {Answer}");
end Part_2;
