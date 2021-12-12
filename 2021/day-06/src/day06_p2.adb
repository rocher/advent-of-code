-----------------------------------------------------------------------------
--
--  Source code generated automatically by 'org-babel-tangle' from
--  file /home/ada/advent-of-code/2021/day-06/README.org
--  2021-12-12 01:31:13
--
--  DO NOT EDIT!!
--
-----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Day06_P2 is
   
   --  __Types__
   subtype Timer_Type is Natural range 0 .. 8;
   
   
   --  __Package_Timer_IO__
   package Timer_IO is new Ada.Text_IO.Integer_IO (Timer_Type);
   
   
   --  __Variables_For_IO__
   Input : File_Type;
   Comma : Character;
   
   
   --  __Variables_For_Counting__
   N           : Natural;
   Tmp         : Long_Integer;
   Timer_Count : array (Timer_Type) of Long_Integer := (others => 0);
   Population  : Long_Integer := 0;
   
begin
   Open (Input, In_File, "/home/ada/advent-of-code/2021/day-06/" & "input");
      
      --  __Read_And_Count_Lanternfish__
      Timer_IO.Get (Input, N);
      loop
         Timer_Count (N) := Timer_Count (N) + 1;
         exit when End_Of_File (Input);
         Get (Input, Comma);
         Timer_IO.Get (Input, N);
      end loop;
      
   Close (Input);
   
   --  __Simulate_256_Days__
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
   end loop;
   
   
   --  __Result_Population__
   for T in Timer_Type loop
      Population := Population + Timer_Count (T);
   end loop;
   
   Put_Line ("Answer:" & Population'Image);
   
end Day06_P2;
