-------------------------------------------------------------------------------
--
--  AOC202308 - Advent of Code 2023 - Day 8 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

procedure Part_1 is
   subtype Labels is String (1 .. 3);

   type Nodes is record
      Left, Right : Labels;
   end record;

   --!pp off
   package Nodes_Package is new Ada.Containers.Hashed_Maps
     (Key_Type        => Labels,
      Element_Type    => Nodes,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   --!pp on

   subtype Nodes_Maps is Nodes_Package.Map;

   Input        : File_Type;
   Answer       : Natural         := 0;
   Filename     : constant String := "input.txt";

   Instructions : Ada.Strings.Unbounded.Unbounded_String;
   Node_Map     : Nodes_Maps;
begin
   Input.Open (In_File, Filename);
   Instructions := Unbounded_IO.Get_Line (Input);
   Input.Skip_Line;
   loop
      declare
         Line : constant String := Input.Get_Line;
         Node : Nodes;
      begin
         Node.Left  := Line (8 .. 10);
         Node.Right := Line (13 .. 15);
         Node_Map.Insert (Line (1 .. 3), Node);
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Node  : Labels  := "AAA";
   Instr : Natural := 0;
   loop
      if Instructions.Element (Instr + 1) = 'L' then
         Node := Node_Map.Element (Node).Left;
      else
         Node := Node_Map.Element (Node).Right;
      end if;
      Answer := @ + 1;
      exit when Node = "ZZZ";
      Instr := (@ + 1) mod Instructions.Length;
   end loop;

   Put_Line (f"Answer: {Answer}");
end Part_1;
