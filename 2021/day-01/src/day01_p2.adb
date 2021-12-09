with Ada.Text_IO; use Ada.Text_IO;

procedure Day01_P2 is
   type Window_Index is mod 3;
   type Measurement_Window is array (Window_Index) of Natural;

   package Measurement_IO is new Integer_IO (Natural);
   use Measurement_IO;

   I     : Window_Index := 0;
   Input : File_Type;

   Input_Window         : Measurement_Window := (others => 0);
   Measurement          : Natural;
   Previous_Measurement : Natural            := Natural'Last;
   Increments           : Natural            := 0;

begin
   Open (Input, In_File, "input");

   --  1st measurement --> 1st window
   Get (Input, Input_Window (0));

   --  2nd measurement --> 1st & 2nd window
   Get (Input, Input_Window (1));
   Input_Window (0) := Input_Window (0) + Input_Window (1);

   --  3rd measurement --> 1st, 2nd & 3rd window
   Get (Input, Input_Window (2));
   Input_Window (0) := Input_Window (0) + Input_Window (2);
   Input_Window (1) := Input_Window (1) + Input_Window (2);
   I := 2;

   --  remaining measurements
   loop
      Measurement := Input_Window (I + 1);
      if Previous_Measurement < Measurement then
         Increments := Increments + 1;
      end if;
      Previous_Measurement := Measurement;

      exit when End_Of_File (Input);

      I := I+1;
      Get (Input, Input_Window (I));
      Input_Window (I - 1) := Input_Window (I - 1) + Input_Window (I);
      Input_Window (I - 2) := Input_Window (I - 2) + Input_Window (I);
   end loop;
   Close (Input);

   Put_Line ("Answer:" & Increments'Image);
end Day01_P2;
