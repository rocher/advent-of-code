with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day01_P2 is
   type Window_Index is mod 3;
   type Measurement_Window is array (Window_Index) of Natural;

   I          : Window_Index    := 0;
   Input      : File_Type;
   Line       : String (1 .. 5) := "     ";
   Line_Count : Natural         := 0;
   Last_Char  : Natural;

   Input_Window         : Measurement_Window := (others => 0);
   Previous_Measurement : Natural            := 0;
   Measurement          : Natural;
   Increments           : Natural            := 0;

   procedure Update_Windows is
      Value : Natural := Natural'Value (Line);
   begin
      Input_Window (I) := Value;
      if Line_Count = 2 then
         Input_Window (I - 1) := Input_Window (I - 1) + Value;
      end if;
      if Line_Count > 2 then
         Input_Window (I - 1) := Input_Window (I - 1) + Value;
         Input_Window (I - 2) := Input_Window (I - 2) + Value;
      end if;
   end Update_Windows;

begin
   Open (Input, In_File, "input");

   while not End_Of_File (Input) loop
      Line_Count := Line_Count + 1;
      Get_Line (Input, Line, Last_Char);
      Update_Windows;

      if Line_Count >= 3 then
         Measurement := Input_Window (I + 1);
         if 0 < Previous_Measurement and Previous_Measurement < Measurement
         then
            Increments := Increments + 1;
         end if;
         Previous_Measurement := Measurement;
      end if;

      I := I + 1;
   end loop;

   Put_Line ("Answer:" & Increments'Image);
end Day01_P2;
