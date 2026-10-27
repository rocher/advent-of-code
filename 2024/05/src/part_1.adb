-------------------------------------------------------------------------------
--
--  AOC202403 - Advent of Code 2024 - Day 3 - Part 1
--  Copyright (c) 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

   type Rule_Type is record
      First, Second : Natural;
   end record;

   package Natural_IO is new Integer_IO (Natural);

   package Rule_Lists is new Ada.Containers.Doubly_Linked_Lists (Rule_Type);
   use Rule_Lists;
   subtype Rule_List is Rule_Lists.List;

   package Page_Lists is new Ada.Containers.Doubly_Linked_Lists (Natural);
   use Page_Lists;
   subtype Page_List is Page_Lists.List;

   use all type Ada.Containers.Count_Type;

   Rules : Rule_List;

   function In_Right_Order
     (Manual : Page_List; Rules : Rule_List) return Boolean
   is
      Current_Page : Page_Lists.Cursor := Manual.First;
      Current_Rule : Rule_Lists.Cursor;

      Page : Natural;
      Rule : Rule_Type;
   begin
      Current_Page := Next (Current_Page);

      loop
         Page := Element (Current_Page);
         Current_Rule := Rules.First;
         loop
            Rule := Element (Current_Rule);

            if Rule.First = Page then
               if Manual.Reverse_Find (Rule.Second, Current_Page)
                 /= Page_Lists.No_Element
               then
                  return False;
               end if;
            end if;

            Current_Rule := Next (Current_Rule);
            exit when not Has_Element (Current_Rule);
         end loop;

         Current_Page := Next (Current_Page);
         exit when not Has_Element (Current_Page);
      end loop;

      return True;
   end In_Right_Order;

begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line : constant String := Input.Get_Line;

         Number, Last : Natural;
         Rule         : Rule_Type;
         Manual       : Page_List;
         Current_Page : Page_Lists.Cursor;
      begin
         if Line'Length = 5 and then Line (3) = '|' then
            Rule.First := Natural'Value (Line (1 .. 2));
            Rule.Second := Natural'Value (Line (4 .. 5));
            Rules.Append (Rule);
         elsif Line'Length > 0 then
            Last := 0;
            loop
               Natural_IO.Get (Line (Last + 1 .. Line'Length), Number, Last);
               Last := Last + 1; --  skip ','
               Manual.Append (Number);
               exit when Last >= Line'Length;
            end loop;
            if In_Right_Order (Manual, Rules) then
               Current_Page := Manual.First;
               for I in 1 .. (Length (Manual) - 1) / 2 loop
                  Current_Page := Next (Current_Page);
               end loop;
               Answer := @ + Element (Current_Page);
            end if;
         end if;
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Put_Line (f"Answer: {Answer}");
end Part_1;
