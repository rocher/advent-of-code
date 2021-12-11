with Ada.Text_IO; use Ada.Text_IO;

procedure Day11_P2 is
   subtype Cave_Rows is Natural range 1 .. 10;
   subtype Cave_Cols is Natural range 1 .. 10;

   type Octopus_Energy is mod 10;
   type Octopus_Cave is array (Cave_Rows, Cave_Cols) of Natural;

   Input       : File_Type;
   Text        : String (1 .. 12);
   Last_Pos    : Natural;
   Row         : Natural;
   Energy      : Octopus_Cave;

   All_Energy  : Natural := 0;
   Step        : Natural := 0;

   procedure Increment (Row : Cave_Rows; Col : Cave_Cols);
   procedure Spread_Flash (Row : Cave_Rows; Col : Cave_Cols);

   procedure Increment (Row : Cave_Rows; Col : Cave_Cols) is
   begin
      Energy (Row, Col) := Energy (Row, Col) + 1;
      if Energy (Row, Col) = 10 then
         Spread_Flash (Row, Col);
      end if;
   end Increment;

   procedure Spread_Flash (Row : Cave_Rows; Col : Cave_Cols) is
   begin
      --  top row
      if Row > Cave_Rows'First then
         if Col > Cave_Cols'First then
            Increment (Row - 1, Col - 1);
         end if;
         Increment (Row - 1, Col);
         if Col < Cave_Cols'Last then
            Increment (Row - 1, Col + 1);
         end if;
      end if;

      --  this row
      if Col > Cave_Cols'First then
         Increment (Row, Col - 1);
      end if;
      if Col < Cave_Cols'Last then
         Increment (Row, Col + 1);
      end if;

      --  bottom row
      if Row < Cave_Rows'Last then
         if Col > Cave_Cols'First then
            Increment (Row + 1, Col - 1);
         end if;
         Increment (Row + 1, Col);
         if Col < Cave_Cols'Last then
            Increment (Row + 1, Col + 1);
         end if;
      end if;
   end Spread_Flash;

begin
   Open (Input, In_File, "input");
   Row := Cave_Rows'First;
   while not End_Of_File (Input) loop
      Get_Line (Input, Text, Last_Pos);
      for Col in Cave_Cols loop
         Energy (Row, Col) := Natural'Value ("0" & Text (Col));
         All_Energy := All_Energy + Energy (Row, Col);
      end loop;
      Row := Row + 1;
   end loop;
   Close (Input);

   Steps :
   while All_Energy > 0 loop
      Step := Step + 1;

      Increment_Octopus_Energy :
      for Row in Cave_Rows loop
         for Col in Cave_Cols loop
            Increment (Row, Col);
         end loop;
      end loop Increment_Octopus_Energy;

      All_Energy := 0;
      Reset_Energy :
      for Row in Cave_Rows loop
         for Col in Cave_Cols loop
            if Energy (Row, Col) > 9 then
               Energy (Row, Col) := 0;
            end if;
            All_Energy := All_Energy + Energy (Row, Col);
         end loop;
      end loop Reset_Energy;

   end loop Steps;

   Put_Line ("Answer:" & Step'Image);
end Day11_P2;
