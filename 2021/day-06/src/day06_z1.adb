-----------------------------------------------------------------------------
--
--  Source code generated automatically by 'org-babel-tangle' from
--  file /home/ada/advent-of-code/2021/day-06/src/day06_z1.adb
--  2022-07-15 19:40:30
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

begin
   Open (Input, In_File, "/home/ada/advent-of-code/2021/day-06/" & "input");

   Close (Input);

   Create (Output, Out_File,  "/home/ada/advent-of-code/2021/day-06/" & "population");
      --  __Simulate_256_Days_And_Write_Population__

      --  population of initial state
      for T in Timer_Type loop
         Population := Population + Timer_Count (T);
      end loop;
      Put_Line (Output, Population'Image);

      loop Per_Day:
      for Day in 1 .. 256 loop
         --  decrement timers
         Tmp := Timer_Count (0);
         for J in Timer_Type'First .. Timer_Type'Last - 1 loop
            Timer_Count (J) := Timer_Count (J + 1);
         end loop;

         --  reset to 6 all timers that reached 0
         Timer_Count (6) := Timer_Count (6) + Tmp;

         --  add new lanternfish per each timer that reached 0
         Timer_Count (Timer_Type'Last) := Tmp;

         --  add new born lanternfish to total population
         Population := Population + Tmp;
         Put_Line (Output_File, Population'Image);
      end loop Per_Day;
   Close (Output);
end Day06_Z1;
