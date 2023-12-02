-------------------------------------------------------------------------------
--
--  AOC202302 - Advent of Code - 2023 day 2
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;

procedure Part_1 is
   Input    : File_Type;
   Answer   : Natural         := 0;
   Filename : constant String := "input.txt";
   Game_Id  : Natural         := 1;

   type Parse_String_Type is record
      Cursor : Natural          := 0;
      String : Unbounded_String := Null_Unbounded_String;
   end record;

   function Next_Token
     (Parse : in out Parse_String_Type) return Unbounded_String
   is
      Slice              : Unbounded_String;
      First, Blank, Last : Natural;
   begin
      return Token : Unbounded_String := Null_Unbounded_String do
         if Parse.Cursor < Parse.String.Length then
            Slice        :=
              Parse.String.Unbounded_Slice
                (Parse.Cursor + 1, Parse.String.Length);
            First        := Slice.Index_Non_Blank;
            Blank        := Slice.Index (" ");
            Last         := (if Blank > 0 then Blank - 1 else Slice.Length);
            Parse.Cursor := @ + Last + 1;
            Token        := Slice.Unbounded_Slice (First, Last);
         end if;
      end return;
   end Next_Token;

begin
   Input.Open (In_File, Filename);
   loop
      declare
         Line  : Parse_String_Type :=
           (0, Ada.Strings.Unbounded.Text_IO.Get_Line (Input));
         Token : Unbounded_String;

         Number   : Natural;
         Color    : Unbounded_String;
         Possible : Boolean := True;
      begin
         Token := Next_Token (Line);  --  skip Game
         Token := Next_Token (Line);  --  skip Id

         Parsing_Line :
         loop
            Token := Next_Token (Line);
            exit Parsing_Line when Token = Null_Unbounded_String;

            Number := Natural'Value (To_String (Token));
            Token  := Next_Token (Line);
            Color  :=
              (if Token.Element (Token.Length) in ',' | ';' then
                 Token.Unbounded_Slice (1, Token.Length - 1)
               else Token);

            if (Color = "red" and then Number > 12)
              or else (Color = "green" and then Number > 13)
              or else (Color = "blue" and then Number > 14)
            then
               Possible := False;
               exit Parsing_Line;
            end if;
         end loop Parsing_Line;

         if Possible then
            Answer := @ + Game_Id;
         end if;
         Game_Id := Game_Id + 1;
      end;

      exit when Input.End_Of_File;
   end loop;
   Input.Close;

   Put_Line ("Answer:" & Answer'Image);
end Part_1;
