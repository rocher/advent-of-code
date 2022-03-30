-----------------------------------------------------------------------------
--
--  Source code generated automatically by 'org-babel-tangle' from
--  file /home/ada/advent-of-code/test/2021/day-06/README.org
--  2022-02-21 19:45:21
--
--  DO NOT EDIT!!
--
-----------------------------------------------------------------------------


with Ada.Text_IO; use Ada.Text_IO;

procedure Day06_Z1 is

   --  __Types__
   subtype Timer_Type is Natural range 0 .. 8;


   --  __Package_Timer_IO__
   package Timer_IO is new Ada.Text_IO.Integer_IO (Timer_Type);


   --  __Variables_For_IO__
   Input : File_Type;
   Comma : Character;


   --  __New_Variables_For_IO__
   Output : File_Type;


   --  __Variables_For_Counting__
   N           : Natural;
   Tmp         : Long_Integer;
   Timer_Count : array (Timer_Type) of Long_Integer := (others => 0);
   Population  : Long_Integer := 0;

begin
   Open (Input, In_File, "/home/ada/advent-of-code/test/2021/day-06/" & "input");

      --  __Read_And_Count_Lanternfish__
      Timer_IO.Get (Input, N);
      loop
         Timer_Count (N) := Timer_Count (N) + 1;
         exit when End_Of_File (Input);
         Get (Input, Comma);
         Timer_IO.Get (Input, N);
      end loop;

   Close (Input);

   Create (Output, Out_File,  "/home/ada/advent-of-code/test/2021/day-06/" & "population");

      --  __Simulate_256_Days_And_Write_Population__

      --  population of initial state
      for T in Timer_Type loop
         Population := Population + Timer_Count (T);
      end loop;
      Put_Line (Output, Population'Image);

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
         Put_Line (Output, Population'Image);
      end loop;

   Close (Output);
end Day06_Z1;
