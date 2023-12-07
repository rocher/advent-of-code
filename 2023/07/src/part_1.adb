-------------------------------------------------------------------------------
--
--  AOC202307 - Advent of Code 2023 - Day 7 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;

with Camel_Cards; use Camel_Cards;

procedure Part_1 is
   package Int_IO renames Ada.Integer_Text_IO;
   package Enum_IO is new Ada.Text_IO.Enumeration_IO (Cards);

   Input    : File_Type;
   Answer   : Long_Integer    := 0;
   Filename : constant String := "input.txt";

   Hands_List : Hands_Lists := Hands_Package.Empty_List;
begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line : constant String := Input.Get_Line;
         Last : Natural;
         Play : Plays;
      begin
         for I in Hands'Range loop
            if Line (I) in '2' .. '9' then
               Enum_IO.Get ("'" & Line (I .. I) & "'", Play.Hand (I), Last);
            else
               Enum_IO.Get (Line (I .. I), Play.Hand (I), Last);
            end if;
         end loop;
         Int_IO.Get (Line (6 .. Line'Last), Play.Bid, Last);
         Hands_List.Append (Play);
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Hands_Sorting.Sort (Hands_List);

   Cursor : Hands_Package.Cursor := Hands_List.First;
   Rank : Natural := 1;
   loop
      exit when not Cursor.Has_Element;
      Answer := @ + Long_Integer (Rank * Cursor.Element.Bid);
      Cursor := Cursor.Next;
      Rank := @ + 1;
   end loop;

   Put_Line (f"Answer: {Answer}");
end Part_1;
