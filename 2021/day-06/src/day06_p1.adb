with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;

procedure Day06_P1 is

   --  __Definition_Of_Timer_Type__
   subtype Timer_Type is Natural range 0 .. 8;
   --  __Package_For_Reading_Input_File__
   package Timer_IO is new Integer_IO (Timer_Type);
   --  __Package_For_Lanternfish_School__
   package Lanternfish_School is new Doubly_Linked_Lists (Timer_Type);
   --  __Variables_For_Reading_Input_File__
   Input_File : File_Type;
   Comma_Char : Character;
   --  __Variables_For_Simulation__
   Timer_Value    : Timer_Type;
   School         : Lanternfish_School.List;
   Fish_Position  : Lanternfish_School.Cursor;
   Resets_Counter : Natural := 0;

begin

   Open (Input_File, In_File, "__Current_Path__" & "input");
      --  __Read_Timers_From_Input_File__
      loop
         Timer_IO.Get (Input_File, Timer_Value);
         School.Append (Timer_Value);
         exit when End_Of_File (Input_File);
         Get (Input_File, Comma_Char);
      end loop;
   Close (Input_File);

   --  __Brute_Force_Simulation_Of_80_Days__
   Per_Day:
   for Day in 1 .. 80 loop

     --  decrement the timer value of each lanternfish
     Fish_Position := School.First;

     Per_Fish:
     while Lanternfish_School.Has_Element (Fish_Position) loop

       Timer_Value := Lanternfish_School.Element (Fish_Position);
       if Timer_Value = 0 then
          Resets_Counter := Resets_Counter + 1;
          School.Replace_Element (Fish_Position, 6);
       else
          School.Replace_Element (Fish_Position, Timer_Value - 1);
       end if;

       Lanternfish_School.Next (Fish_Position);
     end loop Per_Fish;

     --  add all newly hatched lanternfish at once: must be added after the
     --  previous loop has finished to avoid growing the list while iterating
     --  over the elements; all newly hatched lanternfish have a maximum timer
     --  value
     School.Append (Timer_Type'Last, Count_Type (Resets_Counter));
     Resets_Counter := 0;

   end loop Per_Day;
   --  __Write_Population_Size_After_80_Days__
   Put_Line ("Answer:" & School.Length'Image);

end Day06_P1;
