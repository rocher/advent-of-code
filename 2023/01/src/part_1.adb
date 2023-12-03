-------------------------------------------------------------------------------
--
--  AOC202302 - Advent of Code 2023 - Day 1 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Natural         := 0;
   Filename : constant String := "input.txt";

   procedure Get_Digits (Text : String; First, Last : out Natural) is
      Is_First : Boolean := True;
   begin
      for C of Text loop
         if C in '1' .. '9' then
            if Is_First then
               First    := Natural'Value (C & "");
               Last     := First;
               Is_First := False;
            else
               Last := Natural'Value (C & "");
            end if;
         end if;
      end loop;
   end Get_Digits;

begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line        : constant String := Input.Get_Line;
         First, Last : Natural;
      begin
         Get_Digits (Line, First, Last);
         Answer := @ + (First * 10 + Last);
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Put_Line ("Answer:" & Answer'Image);
end Part_1;
