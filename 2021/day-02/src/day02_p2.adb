with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day02_P2 is
   type Position_Type is record
      Depth      : Natural := 0;
      Horizontal : Natural := 0;
      Aim        : Natural := 0;
   end record;

   Input     : File_Type;
   Line      : String (1 .. 16);
   Last_Char : Natural;

   Position : Position_Type;
   Value    : Natural;
   Result   : Natural;

begin
   Open (Input, In_File, "input");

   while not End_Of_File (Input) loop
      Line := (others => ' ');
      Get_Line (Input, Line, Last_Char);
      if Line (1 .. 2) = "up" then
         Position.Aim := Position.Aim - Natural'Value (Line (4 .. 16));
      elsif Line (1 .. 4) = "down" then
         Position.Aim := Position.Aim + Natural'Value (Line (6 .. 16));
      elsif Line (1 .. 7) = "forward" then
         Value               := Natural'Value (Line (9 .. 16));
         Position.Horizontal := Position.Horizontal + Value;
         Position.Depth      := Position.Depth + (Position.Aim * Value);
      end if;
   end loop;

   Result := Position.Depth * Position.Horizontal;
   Put_Line ("Answer:" & Result'Image);
end Day02_P2;
