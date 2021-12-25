with Ada.Text_IO; use Ada.Text_IO;

procedure Day25_P1 is
   Max_Rows : constant Natural := 137;
   Max_Cols : constant Natural := 139;

   Empty        : constant Character := '.';
   East_Facing  : constant Character := '>';
   South_Facing : constant Character := 'v';

   type Row_Type is mod Max_Rows;
   type Col_Type is mod Max_Cols;
   type Sea_Floor_Type is array (Row_Type, Col_Type) of Character;

   Sea_Floor : Sea_Floor_Type;

   Input    : File_Type;
   Text     : String (1 .. Max_Cols + 2);
   Last_Pos : Natural;
   Row      : Row_Type := 0;
   Answer   : Natural  := 1;

   function Move_Sea_Cucumbers
     (Sea_Floor : in out Sea_Floor_Type) return Boolean
   is
      New_Sea_Floor : Sea_Floor_Type := Sea_Floor;
      Steps         : Boolean        := False;
   begin
      for R in Row_Type'Range loop
         for C in Col_Type'Range loop
            if Sea_Floor (R, C) = Empty and Sea_Floor (R, C - 1) = East_Facing
            then
               New_Sea_Floor (R, C)     := East_Facing;
               New_Sea_Floor (R, C - 1) := Empty;
               Steps                    := True;
            end if;
         end loop;
      end loop;
      Sea_Floor := New_Sea_Floor;

      for R in Row_Type'Range loop
         for C in Col_Type'Range loop
            if Sea_Floor (R, C) = Empty and Sea_Floor (R - 1, C) = South_Facing
            then
               New_Sea_Floor (R, C)     := South_Facing;
               New_Sea_Floor (R - 1, C) := Empty;
               Steps                    := True;
            end if;
         end loop;
      end loop;

      Sea_Floor := New_Sea_Floor;
      return Steps;
   end Move_Sea_Cucumbers;

begin
   Open (Input, In_File, "input");
   while not End_Of_File (Input) loop
      Get_Line (Input, Text, Last_Pos);
      for C in 1 .. Max_Cols loop
         Sea_Floor (Row, Col_Type (C - 1)) := Text (C);
      end loop;
      Row := Row + 1;
   end loop;
   Close (Input);

   while Move_Sea_Cucumbers (Sea_Floor) loop
      Answer := Answer + 1;
   end loop;

   Put_Line ("Answer:" & Answer'Image);
end Day25_P1;
