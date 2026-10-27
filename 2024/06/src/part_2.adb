-------------------------------------------------------------------------------
--
--  AOC202403 - Advent of Code 2024 - Day 3 - Part 2
--  Copyright (c) 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_2 is
   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

   subtype Lab_Range is Natural range 1 .. 10;

   type Direction is (North, East, South, West);

   Stepped  : constant Character := 'X';
   Free     : constant Character := '.';
   Obstacle : constant Character := '#';

   type Lab_Position is record
      X, Y : Lab_Range;
   end record;

   type Lab_Type is array (Lab_Range, Lab_Range) of Character;

   type Guard_Type is record
      Pos : Lab_Position;
      Dir : Direction;
   end record;

   Lab     : Lab_Type;
   Y       : Lab_Range;
   Guard   : Guard_Type;
   Turn_90 : constant array (Direction) of Direction :=
     [North => East, East => South, South => West, West => North];

   function Exit_Lab (G : Guard_Type) return Boolean is
   begin
      return Result : Boolean do
         Result :=
           ((G.Pos.X = Lab_Range'First and then G.Dir = West)
            or else (G.Pos.X = Lab_Range'Last and then G.Dir = East)
            or else (G.Pos.Y = Lab_Range'First and then G.Dir = North)
            or else (G.Pos.Y = Lab_Range'Last and then G.Dir = South));
      end return;
   end Exit_Lab;

   procedure Step is
      X : Lab_Range := Guard.Pos.X;
      Y : Lab_Range := Guard.Pos.Y;
   begin
      case Guard.Dir is
         when North =>
            Y := Y - 1;

         when East =>
            X := X + 1;

         when South =>
            Y := Y + 1;

         when West =>
            X := X - 1;
      end case;

      if Lab (X, Y) /= Obstacle then
         Guard.Pos := (X, Y);
      else
         Guard.Dir := Turn_90 (Guard.Dir);
      end if;
   end Step;

   procedure Print_Lab (L : Lab_Type) is
   begin
      for Y in Lab_Range loop
         for X in Lab_Range loop
            Put (L (X, Y));
         end loop;
         New_Line;
      end loop;
   end Print_Lab;

   function Closed_Loop return Boolean is
      X : Lab_Range := Guard.Pos.X;
      Y : Lab_Range := Guard.Pos.Y;
      L : Lab_Type := Lab;
      G : Guard_Type := Guard;
      C : Natural := 0;
   begin
      return Is_Loop : Boolean := False do
         case G.Dir is
            when North =>
               if Lab_Range'First < X and then L (X - 1, Y) /= Obstacle then
                  L (X - 1, Y) := Obstacle;
               else
                  return;
               end if;

            when East =>
               if Lab_Range'First < Y and then L (X, Y - 1) /= Obstacle then
                  L (X, Y - 1) := Obstacle;
               else
                  return;
               end if;

            when South =>
               if X < Lab_Range'Last and then L (X + 1, Y) /= Obstacle then
                  L (X + 1, Y) := Obstacle;
               else
                  return;
               end if;

            when West =>
               if Y < Lab_Range'Last and then L (X, Y + 1) /= Obstacle then
                  L (X, Y + 1) := Obstacle;
               else
                  return;
               end if;
         end case;

         --  if Guard = ((5, 7), North) then
         --  Put_Line (G'Image);
         --  Print_Lab (L);
         --  end if;
         L (G.Pos.X, G.Pos.Y) := Stepped;
         loop
            exit when Exit_Lab (G) or else Is_Loop;

            case G.Dir is
               when North =>
                  Y := Y - 1;

               when East =>
                  X := X + 1;

               when South =>
                  Y := Y + 1;

               when West =>
                  X := X - 1;
            end case;

            if L (X, Y) /= Obstacle then
               G.Pos := (X, Y);
               L (G.Pos.X, G.Pos.Y) := Stepped;
            else
               X := G.Pos.X;
               Y := G.Pos.Y;
               G.Dir := Turn_90 (G.Dir);
               Is_Loop := (L (X, Y) = Stepped);

               case G.Dir is
                  when North =>
                     C := C + (if (L (X, Y - 1) = Stepped) then 1 else 0);

                  when East =>
                     C := C + (if (L (X + 1, Y) = Stepped) then 1 else 0);

                  when South =>
                     C := C + (if (L (X, Y + 1) = Stepped) then 1 else 0);

                  when West =>
                     C := C + (if (L (X - 1, Y) = Stepped) then 1 else 0);
               end case;
            end if;

            --  if Guard = ((8, 7), West) then
            --     --  Put_Line (G'Image);
            --     Print_Lab (L);
            --     New_Line;
            --  end if;

            Is_Loop := (C = 4) or else (G.Pos = Guard.Pos);
            if Is_Loop then
               Put_Line ("Is_Loop @" & X'Image & "," & Y'Image);
            end if;
         end loop;
      end return;
   end Closed_Loop;

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
               Lab (X, Y) := Free;
               Guard := ((X, Y), North);
               Put_Line (Guard'Image);
            end if;
         end loop;
         Y := @ + (if Y = Lab_Range'Last then 0 else 1);
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   loop
      exit when Exit_Lab (Guard);
      Answer := @ + (if Closed_Loop then 1 else 0);
      Step;
   end loop;

   Put_Line (f"Answer: {Answer}");
end Part_2;
