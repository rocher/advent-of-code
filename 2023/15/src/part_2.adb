-------------------------------------------------------------------------------
--
--  AOC202315 - Advent of Code 2023 - Day 15 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;

procedure Part_2 is
   type Lens_Type is record
      Label        : Ada.Strings.Unbounded.Unbounded_String;
      Focal_Length : Natural;
   end record;

   package Lens_Lists is new Doubly_Linked_Lists (Lens_Type);
   use all type Lens_Lists.Cursor;
   subtype Lens_List is Lens_Lists.List;

   Box : array (0 .. 255) of Lens_List := [others => Lens_Lists.Empty_List];

   Input    : File_Type;
   Answer   : Natural         := 0;
   Filename : constant String := "input.txt";

   function Hash (S : String) return Natural is
   begin
      return Result : Natural := 0 do
         for C of S loop
            Result := @ + Character'Pos (C);
            Result := (@ * 17) mod 256;
         end loop;
      end return;
   end Hash;

   procedure Operate_Lens (Str : String) is
      Label_Str : constant String :=
        (if Str (Str'Last) = '-' then Str (Str'First .. Str'Last - 1)
         else Str (Str'First .. Str'Last - 2));

      Label : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String (Label_Str);

      Operator : constant Character :=
        (if Str (Str'Last) = '-' then '-' else '=');

      New_Focal_Length : Natural;

      Box_Index : constant Natural  := Hash (Label_Str);
      Cursor    : Lens_Lists.Cursor := Lens_Lists.No_Element;

      procedure Find_Label (C : Lens_Lists.Cursor) is
         use Ada.Strings.Unbounded;
      begin
         if C.Element.Label = Label then
            Cursor := C;
         end if;
      end Find_Label;
   begin
      --  check if the lens label already exists in the corresponding box
      Box (Box_Index).Iterate (Find_Label'Access);

      if Operator = '-' and then Cursor /= Lens_Lists.No_Element then
         --  remove labelled lens
         Box (Box_Index).Delete (Cursor);
      elsif Operator = '=' then
         New_Focal_Length := Natural'Value ("" & Str (Str'Last));
         if Cursor = Lens_Lists.No_Element then
            --  append lens
            Box (Box_Index).Append
              ((Label => Label, Focal_Length => New_Focal_Length));
         else
            --  replace focal length
            Box (Box_Index).Reference (Cursor).Focal_Length :=
              New_Focal_Length;
         end if;
      end if;
   end Operate_Lens;
begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line  : constant String := Input.Get_Line;
         Start : Natural         := Line'First;
         Index : Natural;
      begin
         loop
            Index := Ada.Strings.Fixed.Index (Line (Start .. Line'Last), ",");
            exit when Index = 0;
            Operate_Lens (Line (Start .. Index - 1));
            Start := Index + 1;
         end loop;
         Operate_Lens (Line (Start .. Line'Last));
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   for I in Box'Range loop
      declare
         Slot   : Natural           := 1;
         Cursor : Lens_Lists.Cursor := Box (I).First;
      begin
         while Cursor.Has_Element loop
            Answer := @ + ((I + 1) * Slot * Cursor.Element.Focal_Length);
            Cursor := Cursor.Next;
            Slot   := @ + 1;
         end loop;
      end;
   end loop;

   Put_Line (f"Answer: {Answer}");
end Part_2;
