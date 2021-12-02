with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day01_P1 is
   Input                : File_Type;
   Line                 : String (1 .. 8);
   Last_Char            : Natural;
   Measurement          : Natural;
   Previous_Measurement : Natural := Natural'Last;
   Increments           : Natural := 0;

begin
   Open (Input, In_File, "input");

   while not End_Of_File (Input) loop
      Line := (others => ' ');
      Get_Line (Input, Line, Last_Char);
      Measurement := Natural'Value (Line);
      if Previous_Measurement < Measurement then
         Increments := Increments + 1;
      end if;
      Previous_Measurement := Measurement;
   end loop;

   Put_Line ("Answer:" & Increments'Image);
end Day01_P1;
