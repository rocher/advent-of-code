with Ada.Strings;                        use Ada.Strings;
with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;

procedure Day06_P1 is
   subtype Timer_Type is Natural range 0 .. 8;

   package Lanterfish_School is new Doubly_Linked_Lists (Timer_Type, "=");
   package Timer_IO is new Ada.Text_IO.Integer_IO (Timer_Type);

   Input : File_Type;
   Comma : Character;

   Timer  : Timer_Type;
   School : Lanterfish_School.List;
   Fish   : Lanterfish_School.Cursor;
   Resets : Natural := 0;

begin
   Open (Input, In_File, "input");

   --  read lanternfish school
   Timer_IO.Get (Input, Timer);
   loop
      School.Append (Timer);
      exit when End_Of_File (Input);
      Get (Input, Comma);
      Timer_IO.Get (Input, Timer);
   end loop;
   Close (Input);

   --  simulate 80 days
   for I in 1 .. 80 loop
      Fish := School.First;
      loop
         Timer := Lanterfish_School.Element (Fish);
         if Timer = 0 then
            Resets := Resets + 1;
            School.Replace_Element (Fish, 6);
         else
            School.Replace_Element (Fish, Timer - 1);
         end if;
         Lanterfish_School.Next (Fish);
         exit when not Lanterfish_School.Has_Element (Fish);
      end loop;
      School.Append (8, Count_Type (Resets));
      Resets := 0;
   end loop;

   Put_Line ("Answer:" & School.Length'Image);
end Day06_P1;
