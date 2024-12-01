-------------------------------------------------------------------------------
--
--  AOC202401 - Advent of Code 2024 - Day 1 - Part 2
--  Copyright (c) 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;

procedure Part_2 is
   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

   package Number_Lists is new Ada.Containers.Doubly_Linked_Lists (Natural);
   use all type Number_Lists.Cursor;
   subtype Number_List is Number_Lists.List;

   Left_List, Right_List : Number_List;

   package Natural_IO is new Integer_IO (Natural);
   package List_Sorting is new Number_Lists.Generic_Sorting;

begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line   : constant String := Input.Get_Line;
         Number : Natural := 0;
         Last   : Positive := 1;
      begin
         Natural_IO.Get (Line (1 .. Line'Last), Number, Last);
         Left_List.Append (Number);
         Natural_IO.Get (Line (Last + 1 .. Line'Last), Number, Last);
         Right_List.Append (Number);
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   List_Sorting.Sort (Left_List);
   List_Sorting.Sort (Right_List);

   declare
      Left  : Number_Lists.Cursor := Left_List.First;
      Right : Number_Lists.Cursor := Number_Lists.No_Element;
      Times : Natural := 0;
   begin
      loop
         --  Count how many times Left.Element appears in Right_List
         Times := 0;
         Right := Right_List.Find (Left.Element);
         if Right /= Number_Lists.No_Element then
            loop
               Times := @ + 1;
               Right := Right.Next;
               exit when Right.Element /= Left.Element;
            end loop;
         end if;
         Answer := @ + (Left.Element * Times);

         --  Skip repeated numbers in Left_List
         declare
            Current_Element : constant Natural := Left.Element;
         begin
            loop
               Left := Left.Next;
               exit when
                 Left = Number_Lists.No_Element
                 or else Left.Element /= Current_Element;
            end loop;
         end;

         exit when not Left.Has_Element;
      end loop;
   end;

   Put_Line (f"Answer: {Answer}");
end Part_2;
