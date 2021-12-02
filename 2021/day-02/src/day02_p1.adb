with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day02_P1 is
   type Position_Type is record
      Depth      : Natural := 0;
      Horizontal : Natural := 0;
   end record;

   Input     : File_Type;
   Line      : String (1 .. 16);
   Last_Char : Natural;

   P : Position_Type;

begin
   Open (Input, In_File, "input");

   while not End_Of_File (Input) loop
      Line := (others => ' ');
      Get_Line (Input, Line, Last_Char);

      if Line (1 .. 2) = "up" then
         P.Depth := P.Depth - Natural'Value (Line (4 .. 16));
      elsif Line (1 .. 4) = "down" then
         P.Depth := P.Depth + Natural'Value (Line (6 .. 16));
      elsif Line (1 .. 7) = "forward" then
         P.Horizontal := P.Horizontal + Natural'Value (Line (9 .. 16));
      end if;
   end loop;

   Put_Line ("Answer:" & Natural'Image (P.Depth * P.Horizontal));
end Day02_P1;
