-------------------------------------------------------------------------------
--
--  AOC202309 - Advent of Code 2023 - Day 9 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Part_1 is
   package Int_IO renames Ada.Integer_Text_IO;

   subtype Index_Type is Natural;

   package Sensor_Data_Package is new Vectors
     (Index_Type => Index_Type, Element_Type => Integer);

   subtype Sensor_Data is Sensor_Data_Package.Vector;

   function "=" (Left, Right : Sensor_Data) return Boolean is
     (Left.Length = Right.Length
      and then
      (for all I in Left.First_Index .. Left.Last_Index =>
         Left (I) = Right (I)));

   package History_Package is new Vectors
     (Index_Type => Index_Type, Element_Type => Sensor_Data);

   subtype History_Vector is History_Package.Vector;

   Input    : File_Type;
   Answer   : Integer         := 0;
   Filename : constant String := "input.txt";

begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line   : constant String := Input.Get_Line;
         Last   : Natural         := 0;
         Number : Integer;

         History : History_Vector;
         Index   : Index_Type;
      begin
         --  read input values
         History.Append (New_Item => [], Count => 1);
         Index := History.First_Index;
         loop
            Last := Last + 1;
            Int_IO.Get (Line (Last .. Line'Last), Number, Last);
            History (Index).Append (Number);
            exit when Last >= Line'Last;
         end loop;

         --  compute history differences
         Index := History.First_Index;
         loop
            History.Append (New_Item => [], Count => 1);
            History (Index + 1).Reserve_Capacity (History (Index).Length + 1);
            for I in
              History (Index).First_Index .. History (Index).Last_Index - 1
            loop
               History (Index + 1).Append
                 (History (Index) (I + 1) - History (Index) (I));
            end loop;
            exit when (for all Data of History (Index) => Data = 0);
            Index := Index + 1;
         end loop;

         --  compute extrapolated value
         for I in reverse History.First_Index .. History.Last_Index - 1 loop
            History (I).Append
              (History (I + 1).Last_Element + History (I).Last_Element);
         end loop;
         Answer := @ + History (History.First_Index).Last_Element;
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Put_Line (f"Answer: {Answer}");
end Part_1;
