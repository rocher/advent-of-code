-------------------------------------------------------------------------------
--
--  AOC202310 - Advent of Code 2023 - Day 10 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_1 is
   --!pp off
   type Pipe_Type is (EW, GND, SW, SE, NW, NE, SOP, NS);
   for Pipe_Type use
     (EW  => Character'Enum_Rep ('-'), --  45
      GND => Character'Enum_Rep ('.'), --  46 -- Ground
      SW  => Character'Enum_Rep ('7'), --  55
      SE  => Character'Enum_Rep ('F'), --  70
      NW  => Character'Enum_Rep ('J'), --  74
      NE  => Character'Enum_Rep ('L'), --  76
      SOP => Character'Enum_Rep ('S'), --  83 -- Start of Path
      NS  => Character'Enum_Rep ('|')  -- 124
   );

   function To_Pipe (C : Character) return Pipe_Type is
   begin
      case C is
         when '-' => return EW;
         when '.' => return GND;
         when '7' => return SW;
         when 'F' => return SE;
         when 'J' => return NW;
         when 'L' => return NE;
         when 'S' => return SOP;
         when '|' => return NS;
         when others => return GND;
      end case;
   end To_Pipe;
   --!pp on

   subtype Maze_Range is Natural range 1 .. 140;
   type Maze_Type is array (Maze_Range, Maze_Range) of Pipe_Type;

   type Maze_Position is record
      X, Y : Natural;
   end record;
   type Maze_Direction is (N, S, E, W);

   type Maze_Path is record
      Pos : Maze_Position;
      Dir : Maze_Direction;
      Len : Natural;
   end record;

   Input    : File_Type;
   Start    : Maze_Position;
   Maze     : Maze_Type;
   Filename : constant String := "input.txt";

   function Follow_Pipe (P : Maze_Path) return Maze_Path is
   begin
      return Path : Maze_Path := P do
         case Path.Dir is
            when N =>
               Path.Pos := (Path.Pos.X, Path.Pos.Y - 1);
               case Maze (Path.Pos.X, Path.Pos.Y) is
                  when SE =>
                     Path.Dir := E;
                  when SW =>
                     Path.Dir := W;
                  when others =>
                     null;
               end case;
            when S =>
               Path.Pos := (Path.Pos.X, Path.Pos.Y + 1);
               case Maze (Path.Pos.X, Path.Pos.Y) is
                  when NE =>
                     Path.Dir := E;
                  when NW =>
                     Path.Dir := W;
                  when others =>
                     null;
               end case;
            when W =>
               Path.Pos := (Path.Pos.X - 1, Path.Pos.Y);
               case Maze (Path.Pos.X, Path.Pos.Y) is
                  when NE =>
                     Path.Dir := N;
                  when SE =>
                     Path.Dir := S;
                  when others =>
                     null;
               end case;
            when E =>
               Path.Pos := (Path.Pos.X + 1, Path.Pos.Y);
               case Maze (Path.Pos.X, Path.Pos.Y) is
                  when NW =>
                     Path.Dir := N;
                  when SW =>
                     Path.Dir := S;
                  when others =>
                     null;
               end case;
         end case;
         Path.Len := @ + 1;
      end return;
   end Follow_Pipe;
begin
   Input.Open (In_File, Filename);
   declare
      Line : String (Maze_Range);
   begin
      for Y in Maze_Range loop
         Line := Input.Get_Line;
         for X in Maze_Range loop
            Maze (X, Y) := To_Pipe (Line (X));
            if Maze (X, Y) = SOP then
               Start := (X, Y);
            end if;
         end loop;
      end loop;
   end;
   Input.Close;

   declare
      I : Natural := 1;

      Path : array (1 .. 2) of Maze_Path := [(Start, N, 0), (Start, N, 0)];
   begin
      if Start.X > Maze_Range'First then
         if Maze (Start.X - 1, Start.Y) in EW | NE | SE then
            Path (I).Dir := W;
            I            := I + 1;
         end if;
      end if;
      if Start.X < Maze_Range'Last then
         if Maze (Start.X + 1, Start.Y) in EW | NW | SW then
            Path (I).Dir := E;
            I            := I + 1;
         end if;
      end if;
      if Start.Y > Maze_Range'First then
         if Maze (Start.X, Start.Y - 1) in NS | SE | SW then
            Path (I).Dir := N;
            I            := I + 1;
         end if;
      end if;
      if Start.Y < Maze_Range'Last then
         if Maze (Start.X, Start.Y + 1) in NS | NE | NW then
            Path (I).Dir := S;
            I            := I + 1;
         end if;
      end if;

      loop
         Path (1) := Follow_Pipe (Path (1));
         Path (2) := Follow_Pipe (Path (2));
         exit when Path (1).Pos = Path (2).Pos;
      end loop;
      Put_Line (f"Answer: {Path(1).Len}");
   end;
end Part_1;
