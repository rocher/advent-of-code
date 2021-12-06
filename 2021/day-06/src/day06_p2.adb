with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day06_P2 is
   subtype Timer_Type is Natural range 0 .. 8;
   type Timer_Count_Type is array (Timer_Type) of Long_Integer;
   package Timer_IO is new Ada.Text_IO.Integer_IO (Timer_Type);

   Input : File_Type;
   Comma : Character;

   N           : Natural;
   T           : Long_Integer;
   Timer_Count : Timer_Count_Type := (others => 0);
   Total       : Long_Integer     := 0;

begin
   Open (Input, In_File, "input");

   --  read lanternfish school
   Timer_IO.Get (Input, N);
   loop
      Timer_Count (N) := Timer_Count (N) + 1;
      Total           := Total + 1;
      exit when End_Of_File (Input);
      Get (Input, Comma);
      Timer_IO.Get (Input, N);
   end loop;
   Close (Input);

   --  simulate 256 days
   for I in 1 .. 256 loop
      T := Timer_Count (0);
      for J in Timer_Type'First .. Timer_Type'Last - 1 loop
         Timer_Count (J) := Timer_Count (J + 1);
      end loop;
      Timer_Count (6)               := Timer_Count (6) + T;
      Timer_Count (Timer_Type'Last) := T;
      Total                         := Total + T;
   end loop;

   Put_Line ("Answer:" & Total'Image);
end Day06_P2;
