-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Almanac_Maps; use Almanac_Maps;

package body Input_Readers is
   package Long_IO is new Ada.Text_IO.Integer_IO (Long_Integer);

   ----------------
   -- Read_Seeds --
   ----------------

   procedure Read_Seeds (Input : File_Type; Seeds : in out Seeds_List) is
      Line   : constant String := Input.Get_Line;
      Number : Long_Integer    := 0;
      Last   : Natural         := 6;
   begin
      loop
         Long_IO.Get (Line (Last + 1 .. Line'Last), Number, Last);
         Seeds.Append (Number);
         exit when Last >= Line'Last;
      end loop;
   end Read_Seeds;

   --------------------
   -- Read_Maps_List --
   --------------------

   procedure Read_Maps_List
     (Input : File_Type; Maps_List : in out Almanac_Maps_List)
   is
   begin
      loop
         declare
            Line   : constant String := Input.Get_Line;
            Number : Long_Integer    := 0;
            Last   : Natural         := 0;
            Map    : Almanac_Map;
         begin
            exit when Input.End_Of_File or else Line = "";

            --  read destination
            Long_IO.Get (Line (Last + 1 .. Line'Last), Number, Last);
            Map.Destination := Number;

            --  read source
            Long_IO.Get (Line (Last + 1 .. Line'Last), Number, Last);
            Map.Source := Number;

            --  read Range
            Long_IO.Get (Line (Last + 1 .. Line'Last), Number, Last);
            Map.Length := Number;

            Maps_List.Append (Map);
         end;
      end loop;
   end Read_Maps_List;

end Input_Readers;
