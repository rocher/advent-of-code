with Ada.Text_IO; use Ada.Text_IO;

procedure Day02_P1 is
   type Position_Type is record
      Depth      : Natural := 0;
      Horizontal : Natural := 0;
   end record;

   Input    : File_Type;
   Text     : String (1 .. 16);
   Last_Pos : Natural;

   P : Position_Type;

begin
   Open (Input, In_File, "input");

   while not End_Of_File (Input) loop
      Text := (others => ' ');
      Get_Line (Input, Text, Last_Pos);

      if Text (1 .. 2) = "up" then
         P.Depth := P.Depth - Natural'Value (Text (4 .. 16));
      elsif Text (1 .. 4) = "down" then
         P.Depth := P.Depth + Natural'Value (Text (6 .. 16));
      elsif Text (1 .. 7) = "forward" then
         P.Horizontal := P.Horizontal + Natural'Value (Text (9 .. 16));
      end if;
   end loop;
   Close (Input);

   Put_Line ("Answer:" & Natural'Image (P.Depth * P.Horizontal));
end Day02_P1;
