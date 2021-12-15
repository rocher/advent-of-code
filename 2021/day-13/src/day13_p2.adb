with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Day13_P2 is
   type Paper_Type is array (0 .. 1_500, 0 .. 900) of Character;
   type Fold_Direction is (Up, Left);

   Input       : File_Type;
   Text        : String (1 .. 32);
   Last_Pos, I : Natural;

   Paper        : Paper_Type := (others => (others => ' '));
   X, Y         : Natural;
   Max_X, Max_Y : Natural    := 0;

   procedure Fold (Left : in out Character; Right : Character) is
   begin
      if Right = '#' then
         Left := '#';
      end if;
   end Fold;

   procedure Fold (Direction : Fold_Direction; Axis : Natural) is
      Dst : Natural := Axis - 1;
      Src : Natural := Axis + 1;
   begin
      loop
         if Direction = Up then
            for X in 0 .. Max_X loop
               Fold (Paper (X, Dst), Paper (X, Src));
            end loop;
         else
            for Y in 0 .. Max_Y loop
               Fold (Paper (Dst, Y), Paper (Src, Y));
            end loop;
         end if;
         exit when Dst = 0 or else (Direction = Up and Src = Max_Y)
           or else (Direction = Left and Src = Max_X);
         Dst := Dst - 1;
         Src := Src + 1;
      end loop;
      if Direction = Up then
         Max_Y := Axis - 1;
      else
         Max_X := Axis - 1;
      end if;
   end Fold;

begin
   Open (Input, In_File, "input");
   while not End_Of_File (Input) loop
      Text := (others => ' ');
      Get_Line (Input, Text, Last_Pos);
      I := Index (Text, ",", 1);
      if I > 0 then
         X            := Natural'Value (Text (1 .. I - 1));
         Y            := Natural'Value (Text (I + 1 .. Last_Pos));
         Max_X        := Natural'Max (X, Max_X);
         Max_Y        := Natural'Max (Y, Max_Y);
         Paper (X, Y) := '#';
      else
         I := Index (Text, "=", 1);
         if I > 0 then
            Fold
              ((if Text (I - 1) = 'y' then Up else Left),
               Natural'Value (Text (I + 1 .. Last_Pos)));
         end if;
      end if;
   end loop;
   Close (Input);

   Put_Line ("Answer:");
   for Y in 0 .. Max_Y loop
      for X in 0 .. Max_X loop
         Put ("" & Paper (X, Y));
      end loop;
      Put_Line ("");
   end loop;
end Day13_P2;
