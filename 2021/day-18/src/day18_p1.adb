with Ada.Text_IO; use Ada.Text_IO;

procedure Day18_P1 is

   function Comma_Index (Text : String) return Natural;


   function Comma_Index (Text : String) return Natural is
      I     : Natural := Text'First;
      Level : Natural := 0;
   begin
      loop
         case (Text (I)) is
            when '[' =>
               Level := Level + 1;
            when ']' =>
               Level := Level - 1;
            when others =>
               null;
         end case;
         exit when Text (I) = ',' and Level = 1;
         I := I + 1;
      end loop;
      return I;
   end Comma_Index;

begin
   --  Insert code here.
   null;
end Day18_P1;
