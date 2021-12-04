with Ada.Strings;                        use Ada.Strings;
with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;

procedure Day04_P2 is
   subtype Row_Range is Natural range 1 .. 5;
   subtype Col_Range is Natural range 1 .. 5;
   subtype Bingo_Number is Natural range 0 .. 99;

   type Row_Mark_Type is array (Row_Range) of Natural;
   type Col_Mark_Type is array (Col_Range) of Natural;
   type Row_Col_Numbers_Type is array (Row_Range, Col_Range) of Bingo_Number;

   type Board_Type is record
      Number       : Row_Col_Numbers_Type;
      Row_Marks    : Row_Mark_Type;
      Col_Marks    : Col_Mark_Type;
      Sum_Unmarked : Natural;
      Bingo        : Boolean;
   end record;
   type Board_Access is access all Board_Type;

   package Boards_Lists is new Doubly_Linked_Lists (Board_Access);
   package Numbers_Lists is new Doubly_Linked_Lists (Bingo_Number);

   Numbers_List : Numbers_Lists.List;
   Boards_List  : Boards_Lists.List;
   Number_Elt   : Numbers_Lists.Cursor;
   Board_Elt    : Boards_Lists.Cursor;
   Number       : Bingo_Number;
   Board        : Board_Access;

   Input     : File_Type;
   Line      : String (1 .. 15);
   Last_Char : Natural;
   Num_Read  : String (1 .. 2);
   Char_Read : Character;
   I         : Natural;
   Answer    : Natural;

   procedure Initialize (Board : in out Board_Access) is
   begin
      Board.Number       := (others => (others => 0));
      Board.Row_Marks    := (others => 0);
      Board.Col_Marks    := (others => 0);
      Board.Sum_Unmarked := 0;
      Board.Bingo        := False;
   end Initialize;

   procedure Set_Number
     (Board : Board_Access; R : Row_Range; C : Col_Range; N : Bingo_Number)
   is
   begin
      Board.Number (R, C) := N;
      Board.Sum_Unmarked  := Board.Sum_Unmarked + N;
   end Set_Number;

   procedure Check_Number (N : Natural; Board : Board_Access) is
   begin
      for R in Row_Range loop
         for C in Col_Range loop
            if Board.Number (R, C) = N then
               Board.Row_Marks (R) := Board.Row_Marks (R) + 1;
               Board.Col_Marks (C) := Board.Col_Marks (C) + 1;
               Board.Sum_Unmarked  := Board.Sum_Unmarked - N;
               if Board.Row_Marks (R) = Row_Range'Last or
                 Board.Col_Marks (C) = Col_Range'Last
               then
                  Board.Bingo := True;
               end if;
            end if;
         end loop;
      end loop;
   end Check_Number;

begin
   Open (Input, In_File, "input");

   --  read numbers list
   I        := 1;
   Num_Read := "  ";
   Numbers_List.Clear;
   Line := (others => ' ');
   while not End_Of_Line (Input) loop
      Get (Input, Char_Read);
      if Char_Read = ',' then
         Numbers_List.Append (Bingo_Number'Value (Num_Read));
         I        := 1;
         Num_Read := "  ";
      else
         Num_Read (I) := Char_Read;
         I            := I + 1;
      end if;
   end loop;
   Numbers_List.Append (Bingo_Number'Value (Num_Read));

   --  skip blank lines
   Get_Line (Input, Line, Last_Char);
   Get_Line (Input, Line, Last_Char);

   --  read boards list
   Read_Boards :
   loop
      Board := new Board_Type;
      Initialize (Board);
      Boards_List.Append (Board);

      for R in Row_Range loop
         Get_Line (Input, Line, Last_Char);
         I := 1;
         for C in Col_Range loop
            Set_Number (Board, R, C, Bingo_Number'Value (Line (I .. I + 1)));
            I := I + 3;
         end loop;
      end loop;

      exit Read_Boards when End_Of_File (Input);

      --  skip blank line
      Get_Line (Input, Line, Last_Char);
   end loop Read_Boards;

   --  process number list
   Number_Elt := Numbers_List.First;
   Process_Numbers :
   loop
      Number := Numbers_Lists.Element (Number_Elt);

      Board_Elt := Boards_List.First;
      Process_Boards :
      loop
         Board := Boards_Lists.Element (Board_Elt);
         if not Board.Bingo then
            Check_Number (Number, Board);
            if Board.Bingo then
               Answer := Number * Board.Sum_Unmarked;
            end if;
         end if;

         Board_Elt := Boards_Lists.Next (Board_Elt);
         exit Process_Boards when not Boards_Lists.Has_Element (Board_Elt);
      end loop Process_Boards;

      Number_Elt := Numbers_Lists.Next (Number_Elt);
      exit Process_Numbers when not Numbers_Lists.Has_Element (Number_Elt);
   end loop Process_Numbers;

   Put_Line ("Answer:" & Answer'Image);
end Day04_P2;
