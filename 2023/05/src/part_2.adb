-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;            use Ada.Text_IO;
with System.Multiprocessors; use System.Multiprocessors;
with System.Multiprocessors.Dispatching_Domains;
use System.Multiprocessors.Dispatching_Domains;

with Almanacs;         use Almanacs;
with Constants;        use Constants;
with Containers.Seeds; use Containers.Seeds;
with Input_Readers;    use Input_Readers;
with Results;          use Results;
with Workers_Tasks;    use Workers_Tasks;

procedure Part_2 is
   --  Domain   : Dispatching_Domain := Create (1, 5);
   Input    : File_Type;
   Answer   : Long_Integer    := Long_Integer'Last;
   Results  : Results_Array;
   Filename : constant String := "input.txt";

   --  list of seeds (1st line)
   Seeds : Seeds_List := Seeds_Lists.Empty_List;

   --  array of lists of almanac maps (rest of file)
   --    1 => seed-to-soil map
   --    2 => soil-to-fertilizer map
   --    ..
   --    7 => humidity-to-location map
   Almanac : aliased Almanacs_Array;
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
      Start, Length : Long_Integer;
      Cursor        : Seeds_Lists.Cursor := Seeds.First;
      CPU           : CPU_Range          := CPU_Range'First;
      Worker        : array (1 .. Seeds_Intervals) of Worker_Task;
   begin
      for I in 1 .. Seeds_Intervals loop
         Start  := Cursor.Element;
         Cursor := Cursor.Next;
         Length := Cursor.Element;
         Cursor := Cursor.Next;

         Set_CPU (CPU, Worker (I)'Identity);
         Worker (I).Initialize
           (I, Almanac'Unrestricted_Access, Start, Length,
            Results'Unrestricted_Access);

         exit when not Cursor.Has_Element;

         if CPU = CPU_Range'Last then
            CPU := CPU_Range'First;
         else
            CPU := CPU + 1;
         end if;
      end loop;
   end;

   --  wait for al workers to finish

   for Result of Results loop
      Answer := Long_Integer'Min (@, Result);
   end loop;
   Put_Line (f"Answer: {Answer}");
end Part_2;
