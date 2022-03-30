with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day03_P1 is
   Report_Number_Width : constant Positive := 12;

   subtype Report_Number_Index is Natural range 1 .. Report_Number_Width;

   Input : File_Type;

   Gamma_Rate    : Natural := 0;
   Epsilon_Rate  : Natural := 0;
   Report_Length : Natural := 0;

   Bits : array (Report_Number_Index) of Natural := (others => 0);

begin
   Open (Input, In_File, "input");

   while not End_Of_File (Input) loop
      declare
         Line : String := Get_Line (Input);
      begin
         Report_Length := Report_Length + 1;

         --  count bits at each position
         for I in Bits'Range loop
            Bits (I) := Bits (I) + Natural'Value ('0' & Line (I));
         end loop;
      end;
   end loop;

   Close (Input);

   for I in Bits'Range loop
      --  find most common bit at each position, and
      --  compute (decimal) gamma and epsilon rates
      if Bits (I) > Report_Length / 2 then
         Gamma_Rate := Gamma_Rate + 2**(Report_Number_Width - I);
      else
         Epsilon_Rate := Epsilon_Rate + 2**(Report_Number_Width - I);
      end if;
   end loop;

   Put_Line ("Answer:" & Natural'Image (Gamma_Rate * Epsilon_Rate));
end Day03_P1;
