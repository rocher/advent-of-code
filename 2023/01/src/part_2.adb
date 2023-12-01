with Ada.Text_IO; use Ada.Text_IO;

procedure Part_2 is
   Input    : File_Type;
   Answer   : Natural         := 0;
   Filename : constant String := "input";

   procedure Get_Digits (Text : String; First, Last : out Natural) is
      Is_First : Boolean := True;
      Index    : Natural := 1;

      procedure Set_Digit (C : Character := '0'; N : Natural := 0) is
         Value : constant Natural :=
           (if C = '0' then N else Natural'Value (C & ""));
      begin
         if Is_First then
            First    := Value;
            Last     := First;
            Is_First := False;
         else
            Last := Value;
         end if;
      end Set_Digit;

      procedure Parse_Digit (Text : String) is
      begin
         case Text is
            when "one" =>
               Set_Digit (N => 1);
            when "two" =>
               Set_Digit (N => 2);
            when "three" =>
               Set_Digit (N => 3);
            when "four" =>
               Set_Digit (N => 4);
            when "five" =>
               Set_Digit (N => 5);
            when "six" =>
               Set_Digit (N => 6);
            when "seven" =>
               Set_Digit (N => 7);
            when "eight" =>
               Set_Digit (N => 8);
            when "nine" =>
               Set_Digit (N => 9);
            when others =>
               null;
         end case;
      end Parse_Digit;

   begin
      for C of Text loop
         if C in '1' .. '9' then
            Set_Digit (C);
         else
            if C in 'o' | 't' | 'f' | 's' | 'e' | 'n' then
               for L in 2 .. 4 loop
                  if Index <= Text'Length - L then
                     Parse_Digit (Text (Index .. Index + L));
                  end if;
               end loop;
            end if;
         end if;
         Index := @ + 1;
      end loop;
   end Get_Digits;

begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line        : constant String := Input.Get_Line;
         First, Last : Natural;
      begin
         Get_Digits (Line, First, Last);
         Answer := @ + (First * 10 + Last);
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Ada.Text_IO.Put_Line ("Answer:" & Answer'Image);
end Part_2;
