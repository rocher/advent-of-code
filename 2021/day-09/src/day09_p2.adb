with Ada.Text_IO; use Ada.Text_IO;

procedure Day09_P2 is
   Row_Max : constant Natural := 100;
   Col_Max : constant Natural := 100;

   subtype Row_Range is Natural range 1 .. Row_Max;
   subtype Col_Range is Natural range 1 .. Col_Max;

   type Height_Map_Type is
     array (0 .. Row_Max + 1, 0 .. Col_Max + 1) of Character;
   type Adjacency_Map_Type is array (1 .. 4) of Character;

   type Largest_Basins_Type is array (1 .. 3) of Natural;

   function Basin_Size
     (Height_Map : in out Height_Map_Type; Row : Natural; Col : Natural)
      return Natural
   is
      Size : Natural := 0;
   begin
      if Height_Map (Row, Col) < '9' then
         Height_Map (Row, Col) := '9';
         Size                  :=
           Basin_Size (Height_Map, Row - 1, Col) +
           Basin_Size (Height_Map, Row + 1, Col) +
           Basin_Size (Height_Map, Row, Col - 1) +
           Basin_Size (Height_Map, Row, Col + 1) + 1;
      end if;
      return Size;
   end Basin_Size;

   procedure Update_Basins
     (Largest_Basin : in out Largest_Basins_Type; Basin : Natural)
   is
   begin
      for I in Largest_Basins_Type'Range loop
         if Basin > Largest_Basin (I) then
            if I < Largest_Basins_Type'Last then
               for J in reverse I + 1 .. Largest_Basins_Type'Last loop
                  Largest_Basin (J) := Largest_Basin (J - 1);
               end loop;
            end if;
            Largest_Basin (I) := Basin;
            exit;
         end if;
      end loop;
   end Update_Basins;

   Input         : File_Type;
   Text          : String (1 .. Row_Max + 2);
   Last_Pos      : Natural;
   Height_Map    : Height_Map_Type     := (others => (others => '9'));
   Basin         : Natural;
   Largest_Basin : Largest_Basins_Type := (others => 0);
   Answer        : Natural             := 1;

begin
   Open (Input, In_File, "input");
   for R in Row_Range loop
      Get_Line (Input, Text, Last_Pos);
      for C in Col_Range loop
         Height_Map (R, C) := Text (C);
      end loop;
   end loop;
   Close (Input);

   for R in Row_Range loop
      for C in Col_Range loop
         Basin := Basin_Size (Height_Map, R, C);
         Update_Basins (Largest_Basin, Basin);
      end loop;
   end loop;

   for I in Largest_Basin'Range loop
      Answer := Answer * Largest_Basin (I);
   end loop;

   Put_Line ("Answer:" & Answer'Image);
end Day09_P2;
