with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

procedure Day14_P2 is
   subtype Element_Type is Character range 'A' .. 'Z';
   type Pair_Insertion_Rule is
     array (Element_Type, Element_Type) of Element_Type;
   type Element_Count is array (Element_Type) of Natural;


begin

end Day14_P2;
