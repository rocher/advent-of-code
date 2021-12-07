with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Containers.Vectors; use Ada.Containers;

procedure Day07_P1 is
   package Position_IO is new Integer_IO (Natural);
   use Position_IO;

   package Naturals is new Vectors
     (Index_Type => Natural, Element_Type => Natural, "=" => "=");
   use Naturals;

   Input    : File_Type;
   Position : Naturals.Vector;
   P        : Natural;
   Comma    : Character;
   Middle   : Natural := 0;
   Answer   : Natural := Natural'Last;

   function Total_Fuel (Target_Position : Natural) return Natural is
      Distance : Natural;
      Total    : Natural := 0;
   begin
      for P in Position.First_Index .. Position.Last_Index loop
         Distance := abs (Position (P) - Target_Position);
         Total    := Total + Distance;
      end loop;
      return Total;
   end Total_Fuel;

begin
   Open (Input, In_File, "input");

   Get (Input, P);
   loop
      Position.Append (P);
      Middle := Middle + P;

      exit when End_Of_File (Input);
      Get (Input, Comma);
      Get (Input, P);
   end loop;
   Close (Input);

   Middle := Middle / Natural (Position.Length);

   declare
      T1, T2    : Natural;
      Direction : Integer := 1;
      Target    : Natural := Middle + Direction;
   begin
      --  search different T1 and T2 near the average
      T1 := Total_Fuel (Middle);
      loop
         T2 := Total_Fuel (Target);
         exit when T1 /= T2;
         Target := Target + Direction;
      end loop;

      --  follow the direction of the lesser
      Direction := (if T1 < T2 then -1 else 1);
      T2        := T1;
      Target    := Middle + Direction;
      loop
         T1 := Total_Fuel (Target);
         exit when T1 >= T2;
         T2     := T1;
         Target := Target + Direction;
      end loop;
      Answer := T2;
   end;

   Put_Line ("Answer:" & Answer'Image);
end Day07_P1;
