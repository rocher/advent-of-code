-------------------------------------------------------------------------------
--
--  AOC202308 - Advent of Code 2023 - Day 8 - Part 2
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with Euler_Tools_Int3;

procedure Part_2 is
   package ETools renames Euler_Tools_Int3;

   subtype Labels is String (1 .. 3);

   type Nodes is record
      Left, Right : Labels;
   end record;

   type Paths is record
      Label : Labels;
      Steps : Natural;
   end record;

   --!pp off
   package Nodes_Maps_Package is new Ada.Containers.Hashed_Maps
     (Key_Type        => Labels,
      Element_Type    => Nodes,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   package Paths_Vector_Package is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Paths);
   --!pp on

   subtype Nodes_Maps is Nodes_Maps_Package.Map;
   subtype Paths_Vectors is Paths_Vector_Package.Vector;

   Input        : File_Type;
   Answer       : ETools.Integer_Type := 1;
   Filename     : constant String     := "input.txt";

   Instructions : Ada.Strings.Unbounded.Unbounded_String;
   Nodes_Map    : Nodes_Maps;          --  input map
   Paths_Vector : Paths_Vectors;       --  nodes labelled 'xxA'
   Nodes_Steps  : ETools.List_Type;    --  path steps from 'xxA' --> 'xxZ'

begin
   Input.Open (In_File, Filename);
   Instructions := Unbounded_IO.Get_Line (Input);
   Input.Skip_Line;
   loop
      declare
         Line  : constant String := Input.Get_Line;
         Node  : Nodes;
         Label : Labels;
         Path  : Paths;
      begin
         Label := Line (1 .. 3);
         Node.Left  := Line (8 .. 10);
         Node.Right := Line (13 .. 15);
         Nodes_Map.Insert (Label, Node);
         if Label (Label'Last) = 'A' then
            Path.Label := Label;
            Path.Steps := 0;
            Paths_Vector.Append (Path);
         end if;
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   for Path of Paths_Vector loop
      Instr : Natural := 0;
      loop
         if Instructions.Element (Instr + 1) = 'L' then
            Path.Label := Nodes_Map.Element (Path.Label).Left;
         else
            Path.Label := Nodes_Map.Element (Path.Label).Right;
         end if;
         Path.Steps := @ + 1;
         exit when Path.Label (Path.Label'Last) = 'Z';
         Instr := (@ + 1) mod Instructions.Length;
      end loop;
      Nodes_Steps.Append (ETools.Integer_Type (Path.Steps));
   end loop;

   Answer := ETools.Least_Common_Multiple (Nodes_Steps);

   Put_Line (f"Answer: {Answer}");
end Part_2;
