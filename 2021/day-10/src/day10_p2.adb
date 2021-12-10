with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors; use Ada.Containers;

procedure Day10_P2 is
   subtype Open_Char is Character with
        Static_Predicate => Open_Char in '(' | '[' | '{' | '<';

   package Characters is new Doubly_Linked_Lists (Character);
   use Characters;

   package Long_Integers is new Vectors (Natural, Long_Integer);
   use Long_Integers;

   package Sorting is new Long_Integers.Generic_Sorting;

   Input     : File_Type;
   Text      : String (1 .. 128);
   Last_Pos  : Natural;
   I         : Natural;
   Corrupted : Boolean;

   Score_Of : array (Character) of Long_Integer :=
     (')' => 1, ']' => 2, '}' => 3, '>' => 4, others => 0);
   Closing_Of : array (Character) of Character :=
     ('(' => ')', '[' => ']', '{' => '}', '<' => '>', others => ' ');

   Navigation_Line : Characters.List;
   K               : Characters.Cursor;
   Line_Score      : Long_Integers.Vector;
   Score           : Long_Integer;
   Answer          : Long_Integer := 0;

begin
   Open (Input, In_File, "input");
   while not End_Of_File (Input) loop
      I         := 1;
      Score     := 0;
      Corrupted := False;
      Text      := (others => ' ');
      Navigation_Line.Clear;
      Get_Line (Input, Text, Last_Pos);

      while not Corrupted and Text (I) /= ' ' loop
         if Text (I) in Open_Char then
            Navigation_Line.Append (Text (I));
         elsif not Navigation_Line.Is_Empty
           and then Closing_Of (Navigation_Line.Last_Element) = Text (I)
         then
            Navigation_Line.Delete_Last;
         else
            Corrupted := True;
         end if;
         I := I + 1;
      end loop;

      if not Corrupted and not Navigation_Line.Is_Empty then
         K := Navigation_Line.Last;
         loop
            Score := (Score * 5) + Score_Of (Closing_Of (Element (K)));
            exit when K = Navigation_Line.First;
            K := Previous (K);
         end loop;
         Line_Score.Append (Score);
      end if;
   end loop;
   Close (Input);

   Sorting.Sort (Line_Score);
   Answer := Line_Score (Natural (Line_Score.Last_Index) / 2);

   Put_Line ("Answer:" & Answer'Image);
end Day10_P2;
