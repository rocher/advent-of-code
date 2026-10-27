-------------------------------------------------------------------------------
--
--  AOC202401 - Advent of Code 2024 - Day 1 - Part 1
--  Copyright (c) 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;

--  with SPARK.Containers;

procedure Part_1 with SPARK_Mode is
   Input_File : File_Type;
   Answer     : Natural := 0;
   Filename   : constant String := "input.txt";

   package Number_Lists is new Ada.Containers.Doubly_Linked_Lists (Natural);
   subtype Number_List is Number_Lists.List;
   use all type Ada.Containers.Count_Type;

   Left_List, Right_List : Number_List;

   package Natural_IO is new Integer_IO (Natural);
   package List_Sorting is new Number_Lists.Generic_Sorting;

   procedure Read_Lists
   with
     Global => (Input => Input_File, In_Out => (Left_List, Right_List)),
     Pre =>
       not Input_File.Is_Open
       and then Left_List.Is_Empty
       and then Right_List.Is_Empty,
     Post =>
       not Input_File.Is_Open
       and then not Left_List.Is_Empty
       and then not Right_List.Is_Empty
       and then Left_List.Length = Right_List.Length
   is
      Number : Natural := 0;
      Last   : Positive := 1;
   begin
      Input_File.Open (In_File, Filename);
      loop
         declare
            Line : constant String := Input_File.Get_Line;
         begin
            Natural_IO.Get (Line (1 .. Line'Last), Number, Last);
            Left_List.Append (Number);
            Natural_IO.Get (Line (Last + 1 .. Line'Last), Number, Last);
            Right_List.Append (Number);
         end;
         exit when Input_File.End_Of_File;
      end loop;
      Input_File.Close;
   end Read_Lists;

begin
   Read_Lists;

   List_Sorting.Sort (Left_List);
   List_Sorting.Sort (Right_List);

   declare
      Left  : Number_Lists.Cursor := Left_List.First;
      Right : Number_Lists.Cursor := Right_List.First;
   begin
      loop
         Answer := @ + abs (Left.Element - Right.Element);
         Left := Left.Next;
         Right := Right.Next;
         exit when not Left.Has_Element;
      end loop;
   end;

   Put_Line (f"Answer: {Answer}");
end Part_1;
