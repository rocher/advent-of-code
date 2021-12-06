with Ada.Text_IO; use Ada.Text_IO;

procedure Day01_P1 is
   package Measurement_IO is new Integer_IO (Natural);
   use Measurement_IO;

   Input                : File_Type;

   Measurement          : Natural;
   Previous_Measurement : Natural := Natural'Last;
   Increments           : Natural := 0;

begin
   Open (Input, In_File, "input");

   Get (Input, Measurement);
   loop
      if Previous_Measurement < Measurement then
         Increments := Increments + 1;
      end if;

      exit when End_Of_File (Input);

      Previous_Measurement := Measurement;
      Get (Input, Measurement);
   end loop;
   Close (Input);

   Put_Line ("Answer:" & Increments'Image);
end Day01_P1;
