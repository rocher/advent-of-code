-----------------------------------------------------------------------------
--
--  Source code generated automatically by 'org-babel-tangle' from
--  file /home/ada/advent-of-code/2021/day-06/src/day06_z1.adb
--  2022-07-20 20:07:43
--
--  DO NOT EDIT!!
--
-----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Day06_Z1 is

   --  __Definition_Of_Timer_Type__
   subtype Timer_Type is Natural range 0 .. 8;
   --  __Package_For_Reading_Input_File__
   package Timer_IO is new Integer_IO (Timer_Type);
   --  __Variables_For_Reading_Input_File__
   Input_File : File_Type;
   Comma_Char : Character;
   --  __Variable_For_Writing_Output_File__
   Output_File : File_Type;
   --  __Variables_For_Counting_Timer_Values__
   Timer_Value : Timer_Type;
   Timer_Zero  : Long_Integer;
   Timer_Count : array (Timer_Type) of Long_Integer := (others => 0);
   Population  : Long_Integer := 0;

begin

   Open (Input_File, In_File, "/home/ada/advent-of-code/2021/day-06/" & "input");
      --  __Read_Input_File_And_Count_Lanternfish__
      loop
         Timer_IO.Get (Input_File, Timer_Value);
         Timer_Count (Timer_Value) := Timer_Count (Timer_Value) + 1;
         exit when End_Of_File (Input_File);
         Get (Input_File, Comma_Char);
      end loop;
   Close (Input_File);

   Create (Output_File, Out_File,  "/home/ada/advent-of-code/2021/day-06/" & "population");
      --  __Simulate_256_Days_And_Write_Population__

      --  population of initial state
      for T in Timer_Type loop
         Population := Population + Timer_Count (T);
      end loop;
      Put_Line (Output_File, Population'Image);

      for Day in 1 .. 256 loop
         --  decrement timers
         Timer_Zero := Timer_Count (0);
         Timer_Count (0 .. 7) := Timer_Count (1 .. 8);

         --  reset to 6 all timers that reached 0
         Timer_Count (6) := Timer_Count (6) + Timer_Zero;

         --  add new lanternfish per each timer that reached 0
         Timer_Count (8) := Timer_Zero;

         --  add newly hatched lanternfish to total population
         Population := Population + Timer_Zero;
         Put_Line (Output_File, Population'Image);
      end loop;
   Close (Output_File);

end Day06_Z1;
