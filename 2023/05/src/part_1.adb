-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

with Containers.Almanac_Maps; use Containers.Almanac_Maps;
with Containers.Seeds;        use Containers.Seeds;
with Input_Readers;           use Input_Readers;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Long_Integer    := Long_Integer'Last;
   Filename : constant String := "input.txt";

   --  list of seeds (1st line)
   Seeds : Seeds_List := Seeds_Lists.Empty_List;

   --  array of lists of almanac maps (rest of file)
   --    1 => seed-to-soil map
   --    2 => soil-to-fertilizer map
   --    ..
   --    7 => humidity-to-location map
   Almanac : array (1 .. 7) of Almanac_Maps_List;

begin
   Input.Open (In_File, Filename);

   Read_Seeds (Input, Seeds);
   Input.Skip_Line;  --  skip blank line

   for I in Almanac'Range loop
      Input.Skip_Line;  --  skip title line
      Read_Maps_List (Input, Almanac (I));
   end loop;
   Input.Close;

   declare
      Destination : Long_Integer;
      Cursor      : Seeds_Lists.Cursor := Seeds.First;
   begin
      loop
         --  Destination = seed
         Destination := Cursor.Element;
         for I in Almanac'Range loop
            Destination := Get_Destination (Almanac (I), Destination);
         end loop;
         --  Destination = location

         Answer := Long_Integer'Min (@, Destination);

         Cursor := Cursor.Next;
         exit when not Cursor.Has_Element;
      end loop;
   end;

   Put_Line (f"Answer: {Answer}");
end Part_1;
