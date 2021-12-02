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

   Position : Position_Type;
   Result   : Natural;

begin
   Open (Input, In_File, "input");

   while not End_Of_File (Input) loop
      Line := (others => ' ');
      Get_Line (Input, Line, Last_Char);
      if Line (1 .. 2) = "up" then
         Position.Depth := Position.Depth - Natural'Value (Line (4 .. 16));
      elsif Line (1 .. 4) = "down" then
         Position.Depth := Position.Depth + Natural'Value (Line (6 .. 16));
      elsif Line (1 .. 7) = "forward" then
         Position.Horizontal :=
           Position.Horizontal + Natural'Value (Line (9 .. 16));
      end if;
   end loop;

   Result := Position.Depth * Position.Horizontal;
   Put_Line ("Answer:" & Result'Image);
end Day02_P1;
