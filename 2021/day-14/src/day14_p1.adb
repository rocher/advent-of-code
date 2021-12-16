with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

procedure Day14_P1 is
   subtype Element_Type is Character range 'A' .. 'Z';
   type Pair_Insertion_Rule is
     array (Element_Type, Element_Type) of Element_Type;
   type Element_Count is array (Element_Type) of Natural;

   Input    : File_Type;
   Text     : String (1 .. 32);
   Last_Pos : Natural;

   Template : String (1 .. 20);
   Rule     : Pair_Insertion_Rule := (others => (others => 'Z'));
   Count    : Element_Count       := (others => 0);
   Max      : Natural             := 0;
   Min      : Natural             := Natural'Last;

   Answer : Natural := 0;

   procedure Apply_Substitution
     (Left : Element_Type; Right : Element_Type; Times : Natural)
   is
      type Turn_Type is mod 2;
      T : Turn_Type := 0;
      E : Element_Type;
      P : Natural;

      Polymer : array (Turn_Type) of String (1 .. 2**Times + 1) :=
        (others => (others => ' '));
   begin
      Polymer (0) (1) := Left;
      Polymer (0) (2) := Right;
      Polymer (1) (1) := Left;

      for I in 1 .. Times loop
         P := 2;
         for K in 2 .. 2**(I - 1) + 1 loop
            E         := Rule (Polymer (T) (K - 1), Polymer (T) (K));
            Count (E) := Count (E) + 1;

            Polymer (T + 1) (P)     := E;
            Polymer (T + 1) (P + 1) := Polymer (T) (K);

            P := P + 2;
         end loop;
         T := T + 1;
      end loop;
   end Apply_Substitution;

begin
   Open (Input, In_File, "input");
   while not End_Of_File (Input) loop
      Text := (others => ' ');
      Get_Line (Input, Text, Last_Pos);
      if Last_Pos > 10 then
         Template := Text (1 .. Last_Pos);
      elsif Last_Pos > 3 then
         Rule (Text (1), Text (2)) := Text (7);
         Count (Text (1))          := Count (Text (1)) + 1;
         Count (Text (2))          := Count (Text (2)) + 1;
      end if;
   end loop;
   Close (Input);

   for I in Template'First + 1 .. Template'Last loop
      Apply_Substitution (Template (I - 1), Template (I), 10);
   end loop;

   for E in Element_Type loop
      if Count (E) > 0 then
         Min := Natural'Min (Min, Count (E));
         Max := Natural'Max (Max, Count (E));
      end if;
   end loop;

   Answer := Max - Min;

   Put_Line ("Answer:" & Answer'Image);
end Day14_P1;
