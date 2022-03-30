-----------------------------------------------------------------------------
--
--  Source code generated automatically by 'org-babel-tangle' from
--  file /home/ada/advent-of-code/test/2021/day-06/README.org
--  2022-02-21 19:45:21
--
--  DO NOT EDIT!!
--
-----------------------------------------------------------------------------


with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;

procedure Day06_P1 is

   --  __Types__
   subtype Timer_Type is Natural range 0 .. 8;


   --  __Package_Timer_IO__
   package Timer_IO is new Ada.Text_IO.Integer_IO (Timer_Type);


   --  __Package_Lanternfish__
   package Lanternfish_School is new Doubly_Linked_Lists (Timer_Type, "=");


   --  __Variables_For_IO__
   Input : File_Type;
   Comma : Character;


   --  __Variables_For_Simulation__
   Timer  : Timer_Type;
   School : Lanternfish_School.List;
   Fish   : Lanternfish_School.Cursor;
   Resets : Natural := 0;

begin
   Open (Input, In_File, "/home/ada/advent-of-code/test/2021/day-06/" & "input");

      -- __Read_Timers__
      Timer_IO.Get (Input, Timer);
      loop
         School.Append (Timer);
         exit when End_Of_File (Input);
         Get (Input, Comma);
         Timer_IO.Get (Input, Timer);
      end loop;

   Close (Input);

   --  __Simulate_80_Days
   for Day in 1 .. 80 loop

      --  decrement timers or current fishes
      Fish := School.First;
      while Lanternfish_School.Has_Element (Fish) loop
         Timer := Lanternfish_School.Element (Fish);
         if Timer = 0 then
            Resets := Resets + 1;
            School.Replace_Element (Fish, 6);
         else
            School.Replace_Element (Fish, Timer - 1);
         end if;
         Lanternfish_School.Next (Fish);
      end loop;

      --  add new born fishes
      School.Append (8, Count_Type (Resets));
      Resets := 0;
   end loop;


   --  __Result__
   Put_Line ("Answer:" & School.Length'Image);

end Day06_P1;
