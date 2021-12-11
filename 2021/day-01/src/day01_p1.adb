-----------------------------------------------------------------------------
--
--  Source code generated automatically by 'org-babel-tangle' from
--  file /home/ada/advent-of-code/2021/day-01/README.org
--  2021-12-11 16:42:39
--
--  DO NOT EDIT!!
--
-----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Day01_P1 is

   -- __Packages__
   package Measurement_IO is new Integer_IO (Natural);
   use Measurement_IO;

   -- __Variables__
   Input                : File_Type;
   Measurement          : Natural;
   Previous_Measurement : Natural := Natural'Last;
   Increments           : Natural := 0;

begin
   Open (Input, In_File, "/home/ada/advent-of-code/2021/day-01/" & "input");
      -- __Detect_Increments__
      Get (Input, Measurement);
      loop
        if Previous_Measurement < Measurement then
           Increments := Increments + 1;
        end if;
      
        exit when End_Of_File (Input);
      
        --  prepare next iteration
        Previous_Measurement := Measurement;
        Get (Input, Measurement);
      end loop;
   Close (Input);

   -- __Result__
   Put_Line ("Answer:" & Increments'Image);
end Day01_P1;
