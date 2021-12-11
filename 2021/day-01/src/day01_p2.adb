-----------------------------------------------------------------------------
--
--  Source code generated automatically by 'org-babel-tangle' from
--  file /home/ada/advent-of-code/2021/day-01/README.org
--  2021-12-11 22:35:32
--
--  DO NOT EDIT!!
--
-----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Day01_P2 is
   
   --  __Types__
   type Window_Index is mod 3;
   type Measurement_Window is array (Window_Index) of Natural;
   
   
   --  __Packages__
   package Measurement_IO is new Integer_IO (Natural);
   use Measurement_IO;
   
   
   --  __Variables__
   Input                : File_Type;
   Measurement          : Natural;
   Previous_Measurement : Natural := Natural'Last;
   Increments           : Natural := 0;
   
   
   --  __New_Variables__
   Input_Window         : Measurement_Window := (others => 0);
   I                    : Window_Index;
   
begin
   Open (Input, In_File, "/home/ada/advent-of-code/2021/day-01/" & "input");
      
      --  __Read_First_Input_Values__
      Get (Input, Input_Window (0));  --  1st measurement --> 1st window
      
      Get (Input, Input_Window (1));  --  2nd measurement --> 1st & 2nd windows
      Input_Window (0) := Input_Window (0) + Input_Window (1);
      
      Get (Input, Input_Window (2));  --  3rd measurement --> 1st, 2nd & 3rd windows
      Input_Window (0) := Input_Window (0) + Input_Window (2);
      Input_Window (1) := Input_Window (1) + Input_Window (2);
      
      
      --  __Process_Rest_Of_Input_Values__
      I := 2;  --  current window
      loop
        Measurement := Input_Window (I + 1);
        if Previous_Measurement < Measurement then
           Increments := Increments + 1;
        end if;
      
        exit when End_Of_File (Input);
      
        --  prepare next itaration
        I := I + 1;
        Get (Input, Input_Window (I));
        Input_Window (I - 1) := Input_Window (I - 1) + Input_Window (I);
        Input_Window (I - 2) := Input_Window (I - 2) + Input_Window (I);
        Previous_Measurement := Measurement;
      end loop;
      
   Close (Input);
   
   --  __Result__
   Put_Line ("Answer:" & Increments'Image);
   
end Day01_P2;
