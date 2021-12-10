with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;

procedure Day10_P1 is
   subtype Open_Char is Character with
        Static_Predicate => Open_Char in '(' | '[' | '{' | '<';

   package Chunk_Package is new Doubly_Linked_Lists (Character);
   use Chunk_Package;

   Input    : File_Type;
   Text     : String (1 .. 128);
   Last_Pos : Natural;
   I        : Natural;

   Syntax_Score : array (Character) of Natural :=
     (')' => 3, ']' => 57, '}' => 1_197, '>' => 25_137, others => 0);
   Closing_Of : array (Character) of Character :=
     ('(' => ')', '[' => ']', '{' => '}', '<' => '>', others => ' ');

   Navigation_Line : Chunk_Package.List;
   Answer          : Natural := 0;

begin
   Open (Input, In_File, "input");
   while not End_Of_File (Input) loop
      I    := 1;
      Text := (others => ' ');
      Navigation_Line.Clear;
      Get_Line (Input, Text, Last_Pos);
      while Text (I) /= ' ' loop
         if Text (I) in Open_Char then
            Navigation_Line.Append (Text (I));
         elsif not Navigation_Line.Is_Empty
           and then Closing_Of (Navigation_Line.Last_Element) = Text (I)
         then
            Navigation_Line.Delete_Last;
         else
            Answer := Answer + Syntax_Score (Text (I));
            exit;
         end if;
         I := I + 1;
      end loop;
   end loop;
   Close (Input);

   Put_Line ("Answer:" & Answer'Image);
end Day10_P1;
