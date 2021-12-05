with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day05_P2 is
   subtype Horizontal_Range is Natural range 0 .. 999;
   subtype Vertical_Range is Natural range 0 .. 999;

   type Ocean_Floor_Type is
     array (Horizontal_Range, Vertical_Range) of Natural;

   type Point_Type is record
      X : Horizontal_Range;
      Y : Vertical_Range;
   end record;

   type Segment_Type is record
      Start_Point, End_Point : Point_Type;
   end record;

   Ocean_Floor_Map : Ocean_Floor_Type := (others => (others => 0));
   Vent            : Segment_Type;

   Input     : File_Type;
   Text      : String (1 .. 24);
   Last_Char : Natural;

   Answer : Natural := 0;

   procedure Get_Next_Value
     (Text : String; Position : in out Natural; Number : out Natural)
   is
      I : Natural := 0;
   begin
      while not (Text (Position) in '0' .. '9') loop
         Position := Position + 1;
      end loop;
      while Text (Position + I) in '0' .. '9' loop
         I := I + 1;
      end loop;
      Number   := Natural'Value ("0" & Text (Position .. Position + I - 1));
      Position := Position + I;
   end Get_Next_Value;

   function Delta_X (S : Segment_Type) return Horizontal_Range is
   begin
      return
        Natural'Max (S.Start_Point.X, S.End_Point.X) -
        Natural'Min (S.Start_Point.X, S.End_Point.X);
   end Delta_X;

   function Delta_Y (S : Segment_Type) return Horizontal_Range is
   begin
      return
        Natural'Max (S.Start_Point.Y, S.End_Point.Y) -
        Natural'Min (S.Start_Point.Y, S.End_Point.Y);
   end Delta_Y;

   function Is_Horizontal (S : Segment_Type) return Boolean is
   begin
      return S.Start_Point.Y = S.End_Point.Y;
   end Is_Horizontal;

   function Is_Vertical (S : Segment_Type) return Boolean is
   begin
      return S.Start_Point.X = S.End_Point.X;
   end Is_Vertical;

   function Is_Straight (S : Segment_Type) return Boolean is
   begin
      return Is_Horizontal (S) or else Is_Vertical (S);
   end Is_Straight;

   function Is_Diagonal (S : Segment_Type) return Boolean is
   begin
      return Delta_X (S) = Delta_Y (S);
   end Is_Diagonal;

   procedure Draw_In_Map (S : Segment_Type) is
      X1, X2 : Horizontal_Range;
      Y1, Y2 : Vertical_Range;
   begin
      if Is_Horizontal (S) then
         X1 := Natural'Min (S.Start_Point.X, S.End_Point.X);
         X2 := Natural'Max (S.Start_Point.X, S.End_Point.X);
         Y2 := S.Start_Point.Y;
         for X in X1 .. X2 loop
            Ocean_Floor_Map (X, Y2) := Ocean_Floor_Map (X, Y2) + 1;
         end loop;
      elsif Is_Vertical (S) then
         X1 := S.Start_Point.X;
         Y1 := Natural'Min (S.Start_Point.Y, S.End_Point.Y);
         Y2 := Natural'Max (S.Start_Point.Y, S.End_Point.Y);
         for Y in Y1 .. Y2 loop
            Ocean_Floor_Map (X1, Y) := Ocean_Floor_Map (X1, Y) + 1;
         end loop;
      elsif Is_Diagonal (S) then
         declare
            Size : Natural := Delta_X (S) + 1;
            Ix   : Integer :=
              (if S.Start_Point.X < S.End_Point.X then 1 else -1);
            Iy : Integer :=
              (if S.Start_Point.Y < S.End_Point.Y then 1 else -1);
            X : Horizontal_Range := S.Start_Point.X;
            Y : Vertical_Range   := S.Start_Point.Y;
         begin
            for I in 1 .. Size loop
               Ocean_Floor_Map (X, Y) := Ocean_Floor_Map (X, Y) + 1;

               if I /= Size then
                  X := X + Ix;
                  Y := Y + Iy;
               end if;
            end loop;
         end;
      end if;
   end Draw_In_Map;

begin
   Open (Input, In_File, "input");

   Process_Input :
   loop
      Text := (others => ' ');
      Get_Line (Input, Text, Last_Char);

      Last_Char := 1;
      Get_Next_Value (Text, Last_Char, Vent.Start_Point.X);
      Get_Next_Value (Text, Last_Char, Vent.Start_Point.Y);
      Get_Next_Value (Text, Last_Char, Vent.End_Point.X);
      Get_Next_Value (Text, Last_Char, Vent.End_Point.Y);

      if Is_Straight (Vent) or else Is_Diagonal (Vent) then
         Draw_In_Map (Vent);
      end if;

      exit Process_Input when End_Of_File (Input);
   end loop Process_Input;

   for X in Horizontal_Range loop
      for Y in Vertical_Range loop
         if Ocean_Floor_Map (X, Y) >= 2 then
            Answer := Answer + 1;
         end if;
      end loop;
   end loop;

   Put_Line ("Answer:" & Answer'Image);
end Day05_P2;
