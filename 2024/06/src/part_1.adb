-------------------------------------------------------------------------------
--
--  AOC202403 - Advent of Code 2024 - Day 3 - Part 1
--  Copyright (c) 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

   subtype Lab_Range is Natural range 1 .. 130;

   type Direction is (North, East, South, West);

   Free     : constant Character := '.';
   Obstacle : constant Character := '#';
   Stepped  : constant Character := 'X';

   type Lab_Position is record
      X, Y : Lab_Range;
   end record;

   type Lab_Type is array (Lab_Range, Lab_Range) of Character;

   type Guard_Type is record
      Pos : Lab_Position;
      Dir : Direction;
   end record;

   Lab   : Lab_Type;
   X, Y  : Lab_Range;
   Guard : Guard_Type :=
     (Pos => (Lab_Range'First, Lab_Range'First), Dir => North);

   function Guard_Exit_Lab return Boolean is
   begin
      return Result : Boolean do
         Result :=
           ((Guard.Pos.X = Lab_Range'First and then Guard.Dir = West)
            or else (Guard.Pos.X = Lab_Range'Last and then Guard.Dir = East)
            or else (Guard.Pos.Y = Lab_Range'First and then Guard.Dir = North)
            or else (Guard.Pos.Y = Lab_Range'Last and then Guard.Dir = South));
      end return;
   end Guard_Exit_Lab;

   function Guard_Step return Natural is
      X : Lab_Range := Guard.Pos.X;
      Y : Lab_Range := Guard.Pos.Y;
   begin
      return Step : Natural := 0 do
         case Guard.Dir is
            when North =>
               if Lab (X, Y - 1) /= Obstacle then
                  Guard.Pos := (X, Y - 1);
                  Step := (if Lab (X, Y - 1) = Free then 1 else 0);
                  Lab (X, Y - 1) := Stepped;
               else
                  Guard.Dir := East;
               end if;

            when East =>
               if Lab (X + 1, Y) /= Obstacle then
                  Guard.Pos := (X + 1, Y);
                  Step := (if Lab (X + 1, Y) = Free then 1 else 0);
                  Lab (X + 1, Y) := Stepped;
               else
                  Guard.Dir := South;
               end if;

            when South =>
               if Lab (X, Y + 1) /= Obstacle then
                  Guard.Pos := (X, Y + 1);
                  Step := (if Lab (X, Y + 1) = Free then 1 else 0);
                  Lab (X, Y + 1) := Stepped;
               else
                  Guard.Dir := West;
               end if;

            when West =>
               if Lab (X - 1, Y) /= Obstacle then
                  Guard.Pos := (X - 1, Y);
                  Step := (if Lab (X - 1, Y) = Free then 1 else 0);
                  Lab (X - 1, Y) := Stepped;
               else
                  Guard.Dir := North;
               end if;
         end case;
      end return;
   end Guard_Step;

begin
   Y := Lab_Range'First;
   Input.Open (In_File, Filename);
   loop
      declare
         Line : constant String := Input.Get_Line;
      begin
         for X in Lab_Range loop
            Lab (X, Y) := Line (X);
            if Lab (X, Y) = '^' then
               Lab (X, Y) := 'X';
               Answer := 1;
               Guard.Pos := (X, Y);
            end if;
         end loop;
         Y := @ + (if Y = Lab_Range'Last then 0 else 1);
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   loop
      exit when Guard_Exit_Lab;
      Answer := @ + Guard_Step;
   end loop;

   Put_Line (f"Answer: {Answer}");
end Part_1;
