with Ada.Strings;            use Ada.Strings;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Containers.Vectors; use Ada.Containers;

procedure Day03_P2 is
   Report_Number_Width : constant Natural := 12;

   subtype Input_Line_Type is String (1 .. Report_Number_Width + 2);
   subtype Report_Number_Index is Natural range 1 .. Report_Number_Width;
   subtype Report_Number_Type is String (Report_Number_Index);

   Input     : File_Type;
   Line      : Input_Line_Type;
   Last_Char : Natural;
   Length    : Natural := 0;

   O2_Generator_Rating : Natural := 0;
   CO2_Srubber_Rating  : Natural := 0;
   Criteria            : array (1 .. 2) of Report_Number_Type;
   Ratings_Found       : Natural := 0;
   Bit_Pos             : Report_Number_Index;
   Count               : Natural;

   function "=" (Left, Right : Report_Number_Type) return Boolean is
      Equals : Boolean             := True;
      I      : Report_Number_Index := Report_Number_Index'First;
   begin
      while Equals and Left (I) /= ' ' and Right (I) /= ' ' loop
         Equals := (Left (I) = Right (I));
         if I < Report_Number_Index'Last then
            I := I + 1;
         else
            exit;
         end if;
      end loop;
      return Equals;
   end "=";

   package Report_Type is new Vectors
     (Index_Type => Natural, Element_Type => Report_Number_Type);
   use Report_Type;

   Report : Report_Type.Vector;

   function Get_Element
     (Criteria : Report_Number_Type) return Report_Number_Type
   is
      P : Report_Type.Cursor := No_Element;
   begin
      P := Report.Find (Criteria);
      return Report.Element (To_Index (P));
   end Get_Element;

   procedure Count_Bits
     (Bit_Pos : Natural; Criteria : Report_Number_Type; Count : out Natural;
      Length  : out Natural)
   is
      Report_Number : Report_Number_Type;
   begin
      Count  := 0;
      Length := 0;
      for I in Report.First_Index .. Report.Last_Index loop
         Report_Number := Report.Element (I);
         if Report_Number = Criteria then
            Length := Length + 1;
            Count := Count + Natural'Value ("0" & Report_Number (Bit_Pos));
         end if;
      end loop;
   end Count_Bits;

   function To_Report_Number (Line : Input_Line_Type) return Report_Number_Type
   is
      Report_Number : Report_Number_Type;
   begin
      Report_Number := Line (Report_Number_Index);
      return Report_Number;
   end To_Report_Number;

   function To_Natural (Bits : Report_Number_Type) return Natural is
   begin
      return Natural'Value ("2#" & Bits & "#");
   end To_Natural;

begin
   Open (Input, In_File, "input");
   while not End_Of_File (Input) loop
      Get_Line (Input, Line, Last_Char);
      Report.Append (To_Report_Number (Line));
   end loop;
   Close (Input);

   Bit_Pos  := Report_Number_Index'First;
   Criteria := (others => (others => ' '));

   while Ratings_Found < 2 loop
      --  O2 generator rating criteria
      if O2_Generator_Rating = 0 then
         Count_Bits (Bit_Pos, Criteria (1), Count, Length);
         if Length = 1 then
            O2_Generator_Rating := To_Natural (Get_Element (Criteria (1)));
            Ratings_Found       := Ratings_Found + 1;
         else
            if Count >= (Length + 1) / 2 then
               Criteria (1) (Bit_Pos) := '1';
            else
               Criteria (1) (Bit_Pos) := '0';
            end if;
            if Bit_Pos = Report_Number_Index'Last then
               O2_Generator_Rating := To_Natural (Criteria (1));
               Ratings_Found       := Ratings_Found + 1;
            end if;
         end if;
      end if;

      -- CO2 scrubber rating criteria
      if CO2_Srubber_Rating = 0 then
         Count_Bits (Bit_Pos, Criteria (2), Count, Length);
         if Length = 1 then
            CO2_Srubber_Rating := To_Natural (Get_Element (Criteria (2)));
            Ratings_Found      := Ratings_Found + 1;
         else
            if Count >= (Length + 1) / 2 then
               Criteria (2) (Bit_Pos) := '0';
            else
               Criteria (2) (Bit_Pos) := '1';
            end if;
            if Bit_Pos = Report_Number_Index'Last then
               CO2_Srubber_Rating := To_Natural (Criteria (2));
               Ratings_Found      := Ratings_Found + 1;
            end if;
         end if;
      end if;

      if Bit_Pos < Report_Number_Index'Last then
         Bit_Pos := Bit_Pos + 1;
      end if;
   end loop;

   Put_Line
     ("Answer:" & Natural'Image (O2_Generator_Rating * CO2_Srubber_Rating));
end Day03_P2;
