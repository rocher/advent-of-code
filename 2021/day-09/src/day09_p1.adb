with Ada.Text_IO; use Ada.Text_IO;

procedure Day09_P1 is
   Row_Max : constant Natural := 100;
   Col_Max : constant Natural := 100;

   subtype Row_Range is Natural range 1 .. Row_Max;
   subtype Col_Range is Natural range 1 .. Col_Max;

   type Height_Map_Type is
     array (0 .. Row_Max + 1, 0 .. Col_Max + 1) of Character;
   type Adjacency_Map_Type is array (1 .. 4) of Character;

   procedure Get_Adjacency_Map
     (Height_Map    :     Height_Map_Type; Row : Row_Range; Col : Col_Range;
      Adjacency_Map : out Adjacency_Map_Type)
   is
   begin
      Adjacency_Map (1) := Height_Map (Row - 1, Col);
      Adjacency_Map (2) := Height_Map (Row + 1, Col);
      Adjacency_Map (3) := Height_Map (Row, Col + 1);
      Adjacency_Map (4) := Height_Map (Row, Col - 1);
   end Get_Adjacency_Map;

   function Min_Adjacent (Adjacency_Map : Adjacency_Map_Type) return Character
   is
      Min : Character := '9';
   begin
      for I in Adjacency_Map_Type'Range loop
         if Adjacency_Map (I) < Min then
            Min := Adjacency_Map (I);
         end if;
      end loop;
      return Min;
   end Min_Adjacent;

   function Risk_Level
     (Height_Map : Height_Map_Type; Row : Row_Range; Col : Col_Range)
      return Natural
   is
      Risk          : Natural := 0;
      Adjacency_Map : Adjacency_Map_Type;
   begin
      Get_Adjacency_Map (Height_Map, Row, Col, Adjacency_Map);
      if Height_Map (Row, Col) < Min_Adjacent (Adjacency_Map) then
         Risk := Natural'Value ("0" & Height_Map (Row, Col)) + 1;
      end if;
      return Risk;
   end Risk_Level;

   Input      : File_Type;
   Text       : String (1 .. Row_Max + 2);
   Last_Pos   : Natural;
   Height_Map : Height_Map_Type := (others => (others => '9'));
   Answer     : Natural         := 0;

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
         Answer := Answer + Risk_Level (Height_Map, R, C);
      end loop;
   end loop;

   Put_Line ("Answer:" & Answer'Image);
end Day09_P1;
