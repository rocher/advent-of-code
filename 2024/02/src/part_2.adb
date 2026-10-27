-------------------------------------------------------------------------------
--
--  AOC202401 - Advent of Code 2024 - Day 1 - Part 2
--  Copyright (c) 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Part_2 is
   package Natural_IO is new Integer_IO (Natural);

   Input    : File_Type;
   Answer   : Natural := 0;
   Filename : constant String := "input.txt";

   type Level_Trend is (decrement, increment);
   type Level_Array is array (1 .. 10) of Natural;

   type Report_Type is record
      Level  : Level_Array;
      Length : Natural;
   end record;

   function Get_Trend
     (Previous_Level, Current_Level : Natural) return Level_Trend
   is (if Previous_Level > Current_Level then decrement else increment);

   function Is_Safe
     (Previous_Level, Current_Level : Natural;
      Previous_Trend, Current_Trend : Level_Trend) return Boolean
   is (Current_Trend = Previous_Trend
       and then abs (Previous_Level - Current_Level) >= 1
       and then abs (Previous_Level - Current_Level) <= 3);

   function Is_Safe (Report : Report_Type) return Boolean is
      I : Natural := 2;

      Previous_Trend, Current_Trend : Level_Trend;
   begin
      return Is_Safe : Boolean := True do
         Current_Trend :=
           (if Report.Level (1) > Report.Level (2) then decrement
            else increment);

         Is_Safe :=
           abs (Report.Level (1) - Report.Level (2)) >= 1
           and then abs (Report.Level (1) - Report.Level (2)) <= 3;
         loop
            exit when not Is_Safe or else I >= Report.Length;
            Previous_Trend := Current_Trend;
            I := I + 1;
            Current_Trend :=
              (if Report.Level (I - 1) > Report.Level (I) then decrement
               else increment);

            Is_Safe :=
              Current_Trend = Previous_Trend
              and then abs (Report.Level (I - 1) - Report.Level (I)) >= 1
              and then abs (Report.Level (I - 1) - Report.Level (I)) <= 3;
         end loop;
      end return;
   end Is_Safe;

   function Remove_Element
     (Index : Natural; Report : Report_Type) return Report_Type is
   begin
      return Repo : Report_Type do
         Repo.Length := 0;
         for I in 1 .. Report.Length loop
            if I /= Index then
               Repo.Length := @ + 1;
               Repo.Level (Repo.Length) := Report.Level (I);
            end if;
         end loop;
      end return;
   end Remove_Element;

begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line : constant String := Input.Get_Line;
         Last : Natural := 0;

         Report : Report_Type;
      begin
         Report.Length := 0;
         loop
            exit when Last >= Line'Last;
            Report.Length := @ + 1;
            Natural_IO.Get
              (Line (Last + 1 .. Line'Last),
               Report.Level (Report.Length),
               Last);
         end loop;

         if Is_Safe (Report) then
            Answer := @ + 1;
         else
            for I in 1 .. Report.Length loop
               if Is_Safe (Remove_Element (I, Report)) then
                  Answer := @ + 1;
                  exit;
               end if;
            end loop;
         end if;
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Put_Line (f"Answer: {Answer}");
end Part_2;
