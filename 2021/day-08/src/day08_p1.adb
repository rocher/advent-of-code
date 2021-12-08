with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Day08_P1 is
   Input    : File_Type;
   Text     : String (1 .. 128);
   Last_Pos : Natural;
   Length   : Natural;
   Answer   : Natural := 0;

begin
   Open (Input, In_File, "input");
   while not End_Of_File (Input) loop
      Text := (others => ' ');
      Get_Line (Input, Text, Last_Pos);
      Last_Pos := 62;
      for I in 1 .. 4 loop
         Length := 0;
         while Text (Last_Pos) /= ' ' loop
            Last_Pos := Last_Pos + 1;
            Length := Length + 1;
         end loop;
         Last_Pos := Last_Pos + 1;
         if Length = 2 or Length = 3 or Length = 4 or Length = 7 then
            Answer := Answer + 1;
         end if;
      end loop;
   end loop;
   Close (Input);

   Put_Line ("Answer:" & Answer'Image);
end Day08_P1;
