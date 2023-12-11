-------------------------------------------------------------------------------
--
--  AOC202310 - Advent of Code 2023 - Day 10 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_2 is
   --!pp off
   type Pipe_Type is (EW, GND, SW, SE, NW, NE, SOP, MOL, NIL, ENC, NS);
   for Pipe_Type use
     (EW  => Character'Enum_Rep ('-'), --  45
      GND => Character'Enum_Rep ('.'), --  46 -- Ground
      SW  => Character'Enum_Rep ('7'), --  55
      SE  => Character'Enum_Rep ('F'), --  70
      NW  => Character'Enum_Rep ('J'), --  74
      NE  => Character'Enum_Rep ('L'), --  76
      SOP => Character'Enum_Rep ('S'), --  83 -- Start of Path
      MOL => Character'Enum_Rep ('x'), -- 120 -- Mark of the Loop
      NIL => Character'Enum_Rep ('y'), -- 121 -- Not in the Loop
      ENC => Character'Enum_Rep ('z'), -- 122 -- Enclosed by the Loop
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
   subtype Maze_Extended_Range is
     Natural range Maze_Range'First - 1 .. Maze_Range'Last + 1;
   type Maze_Type is
     array (Maze_Extended_Range, Maze_Extended_Range) of Pipe_Type;

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
   Maze     : Maze_Type       := [others => [others => GND]];
   Flood    : Maze_Type       := [others => [others => GND]];
   Answer   : Natural         := 0;
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

   function Is_Near_NIL (X, Y : Maze_Range) return Boolean is
   begin
      return
        Flood (X - 1, Y) = NIL or else Flood (X + 1, Y) = NIL
        or else Flood (X, Y - 1) = NIL or else Flood (X, Y + 1) = NIL;
   end Is_Near_NIL;

   procedure Flood_Fill (Px, Py : Maze_Range; In_Reverse : Boolean := False) is
   begin
      if Flood (Px, Py) /= GND then
         return;
      end if;
      Flood (Px, Py) := NIL;
      if In_Reverse then
         for Y in reverse Maze_Range'First .. Py loop
            for X in Maze_Range'First .. Px loop
               if Flood (X, Y) = GND and then Is_Near_NIL (X, Y) then
                  Flood (X, Y) := NIL;
               end if;
            end loop;
         end loop;
      else
         for Y in reverse Maze_Range'First .. Py loop
            for X in Px .. Maze_Range'Last loop
               if Flood (X, Y) = GND and then Is_Near_NIL (X, Y) then
                  Flood (X, Y) := NIL;
               end if;
            end loop;
         end loop;
      end if;
   end Flood_Fill;

begin
   Input.Open (In_File, Filename);
   declare
      Line : String (Maze_Range);
   begin
      for Y in Maze_Range loop
         Line := Input.Get_Line;
         for X in Maze_Range loop
            --  Maze (X, Y) := Pipe_Type'Val (Character'Enum_Rep (Line (X)));
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
      --  select start directions
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

      --  mark the loop in 'Flood'
      Flood (Start.X, Start.Y) := MOL;  --  Mark of Loop
      loop
         Path (1) := Follow_Pipe (Path (1));
         Path (2) := Follow_Pipe (Path (2));

         Flood (Path (1).Pos.X, Path (1).Pos.Y) := MOL;
         Flood (Path (2).Pos.X, Path (2).Pos.Y) := MOL;

         exit when Path (1).Pos = Path (2).Pos;
      end loop;
   end;

   --  remove all junk pipes
   for Y in Maze_Range loop
      for X in Maze_Range loop
         if Flood (X, Y) /= MOL then
            Flood (X, Y) := GND;  --  Ground
         end if;
      end loop;
   end loop;

   --  Flood fill: all external GND --> NIL (Not in Loop)
   for Y in Maze_Range loop
      if Flood (Maze_Range'First, Y) = GND then
         Flood_Fill (Maze_Range'First, Y);
      end if;
      if Flood (Maze_Range'Last, Y) = GND then
         Flood_Fill (Maze_Range'Last, Y, True);
      end if;
   end loop;
   for X in Maze_Range loop
      if Flood (X, Maze_Range'First) = GND then
         Flood_Fill (X, Maze_Range'First);
      end if;
      if Flood (X, Maze_Range'Last) = GND then
         Flood_Fill (X, Maze_Range'Last, True);
      end if;
   end loop;

   --  remove all junk pipes from 'Maze'
   for Y in Maze_Range loop
      for X in Maze_Range loop
         if Flood (X, Y) in GND | NIL then
            Maze (X, Y) := Flood (X, Y);
         end if;
      end loop;
   end loop;

   --  count enclosed GND points (parity odd)
   for Y in Maze_Range loop
      for X in Maze_Range loop
         if Maze (X, Y) = GND then
            Parity : Natural := 0;
            for Dx in X + 1 .. Maze_Range'Last loop
               if Maze (Dx, Y) in NS | NE | NW | MOL then
                  Parity := @ + 1;
               end if;
            end loop;
            Answer := @ + (if Parity mod 2 = 0 then 0 else 1);
         end if;
      end loop;
   end loop;

   Put_Line (f"Answer: {Answer}");
end Part_2;
