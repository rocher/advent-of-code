with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Day08_P2 is
   type Segmented_Digit is new String (1 .. 7);
   subtype Digit_Value is Natural range 0 .. 9;

   function Digit_Hash (Digit : Segmented_Digit) return Hash_Type is
      Result : Hash_Type := Hash_Type'First;
   begin
      return Result;
   end Digit_Hash;

   function Digit_Equivalents (Left, Right : Segmented_Digit) return Boolean is
     (Left = Right);

   package Digit_Package is new Hashed_Maps
     (Key_Type => Segmented_Digit, Element_Type => Digit_Value,
      Hash     => Digit_Hash, Equivalent_Keys => Digit_Equivalents);
   use Digit_Package;

   function Contains (Number, Pattern : Segmented_Digit) return Boolean is
      Pos    : Natural := 1;
      Result : Boolean := True;
   begin
      loop
         Result :=
           (Index (String (Number), String (Pattern (Pos .. Pos))) /= 0);
         Pos := Pos + 1;
         exit when Pos > 7 or else (Pattern (Pos) = ' ' or Result = False);
      end loop;
      return Result;
   end Contains;

   function Find_Key
     (Digit_Map : Map; Digit : Segmented_Digit) return Segmented_Digit
   is
      C : Cursor := Digit_Map.First;
      K : Segmented_Digit;
   begin
      loop
         K := Key (C);
         if Contains (K, Digit) and Contains (Digit, K) then
            return K;
         end if;
         C := Next (C);
      end loop;
   end Find_Key;

   procedure Decode_Digits (Digit_Map : in out Map; Text : String) is
      Digit_Read : Segmented_Digit;
      Pos        : Natural := 1;
      Length     : Natural;

      Code_1478 : array (1 .. 4) of Segmented_Digit :=
        (others => (others => ' '));
      Code_069 : array (1 .. 3) of Segmented_Digit :=
        (others => (others => ' '));
      Code_235 : array (1 .. 3) of Segmented_Digit :=
        (others => (others => ' '));

      Map_069                  : array (1 .. 3) of Segmented_Digit;
      Counter_069, Counter_235 : Natural := 1;

      Code : array (0 .. 9) of Segmented_Digit := (others => (others => ' '));

   begin
      Digit_Map.Clear;

      --  read all sequences in the given text
      for I in 0 .. 9 loop
         Length     := 0;
         Digit_Read := (others => ' ');
         while Text (Pos) /= ' ' loop
            Length              := Length + 1;
            Digit_Read (Length) := Text (Pos);
            Pos                 := Pos + 1;
         end loop;
         Pos := Pos + 1;
         -- Put (String (Digit_Read) & ":" & Length'Image & ", ");

         -- assign values depending on length
         case Length is
            when 2 =>
               Code_1478 (1) := Digit_Read;
               Digit_Map.Insert (Digit_Read, 1);
            when 3 =>
               Code_1478 (3) := Digit_Read;
               Digit_Map.Insert (Digit_Read, 7);
            when 4 =>
               Code_1478 (2) := Digit_Read;
               Digit_Map.Insert (Digit_Read, 4);
            when 5 =>
               Code_235 (Counter_235) := Digit_Read;
               Counter_235            := Counter_235 + 1;
            when 6 =>
               Code_069 (Counter_069) := Digit_Read;
               Counter_069            := Counter_069 + 1;
            when 7 =>
               Code_1478 (4) := Digit_Read;
               Digit_Map.Insert (Digit_Read, 8);
            when others =>
               null;
         end case;
      end loop;

      --  rest of values: 0, 6 and 9 deduced from previous
      for I in 1 .. 3 loop
         declare
            C4, C1 : Boolean;
         begin
            C1 := Contains (Code_069 (I), Code_1478 (1));
            C4 := Contains (Code_069 (I), Code_1478 (2));
            if C4 then
               Digit_Map.Insert (Code_069 (I), 9);
               Map_069 (3) := Code_069 (I);
            elsif C1 then
               Digit_Map.Insert (Code_069 (I), 0);
               Map_069 (1) := Code_069 (I);
            else
               Digit_Map.Insert (Code_069 (I), 6);
               Map_069 (2) := Code_069 (I);
            end if;
         end;
      end loop;

      --  rest of values: 2, 3 and 5 deduced from previous
      for I in 1 .. 3 loop
         declare
            C7, C6 : Boolean;
         begin
            C7 := Contains (Code_235 (I), Code_1478 (3));
            if C7 then
               Digit_Map.Insert (Code_235 (I), 3);
            else
               C6 := Contains (Map_069 (2), Code_235 (I));
               if C6 then
                  Digit_Map.Insert (Code_235 (I), 5);
               else
                  Digit_Map.Insert (Code_235 (I), 2);
               end if;
            end if;
         end;
      end loop;
   end Decode_Digits;

   Input    : File_Type;
   Text     : String (1 .. 128);
   Last_Pos : Natural;
   Length   : Natural;

   Digit_Map : Map;
   Digit     : Segmented_Digit;
   Value     : Natural;
   Answer    : Natural := 0;

begin
   Open (Input, In_File, "input");
   while not End_Of_File (Input) loop
      Last_Pos := 0;
      Text     := (others => ' ');
      Get_Line (Input, Text, Last_Pos);
      Decode_Digits (Digit_Map, Text);

      Last_Pos := 62;
      for I in 1 .. 4 loop
         Length := 1;
         Digit  := (others => ' ');
         while Text (Last_Pos) /= ' ' loop
            Digit (Length) := Text (Last_Pos);
            Last_Pos       := Last_Pos + 1;
            Length         := Length + 1;
         end loop;
         Value    := Digit_Map (Find_Key (Digit_Map, Digit)) * (10**(4 - I));
         Answer   := Answer + Value;
         Last_Pos := Last_Pos + 1;
      end loop;
   end loop;
   Close (Input);

   Put_Line ("Answer:" & Answer'Image);
end Day08_P2;
