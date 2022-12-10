-- --------------------------------------------------------------------------
--
--  SPDX-License-Identifier: CC-BY-NC-ND-4.0
--  SPDX-FileCopyrightText: Copyright 2022 Francesc Rocher
--  SPDX-Creator: Francesc Rocher (francesc.rocher@gmail.com)
--  Ref: https://creativecommons.org/licenses/by-nc-nd/4.0/deed.en
--
-- --------------------------------------------------------------------------
--
--  Source code generated automatically by 'org-babel-tangle' from
--  file /home/ada/advent-of-code/public/2022/10/README.org
--  2022-12-10 23:36:53
--
--  DO NOT EDIT!!
--
-- --------------------------------------------------------------------------

pragma Ada_2022;

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Wide_Text_IO;

procedure Day10_P2 is
   --  __CPU_Model__
   X     : Integer := 1;
   Cycle : Positive := 1;

   Code  : String (1 .. 4);
   Value : Integer;

   Input : File_Type;
   --  __CRT_Model__
   subtype Row_Range is Integer range 1 .. 6;
   subtype Col_Range is Integer range 0 .. 39;

   CRT : array (Row_Range, Col_Range) of Wide_Character;

   -- current pixel coordinates
   CRT_Row : Row_Range := 1;
   CRT_Col : Col_Range := 0;

  --  __Pixel_Dynamics__
  procedure Next_Pixel is
  begin
     if CRT_Col < Col_Range'Last then
        CRT_Col := @ + 1;
     else
        CRT_Col := 0;
        if CRT_Row < Row_Range'Last then
           CRT_Row := @ + 1;
        end if;
     end if;
  end Next_Pixel;
  --  __Pixel_Drawing__
  procedure Draw_Pixel (Sprite : Integer) is
  begin
     if CRT_Col in Sprite - 1 .. Sprite + 1 then
        CRT (CRT_Row, CRT_Col) := '█';
     else
        CRT (CRT_Row, CRT_Col) := ' ';
     end if;
  end Draw_Pixel;
  --  __CRT_Output__
  procedure Draw_CRT is
  begin
     for Row in Row_Range loop
        for Col in Col_Range loop
             Ada.Wide_Text_IO.Put (CRT (Row, Col));
        end loop;
        if Row < Row_Range'Last then
           New_Line;
        end if;
     end loop;
  end Draw_CRT;

begin
    Open (Input, In_File, "/home/ada/advent-of-code/public/2022/10/" & "input.txt");

    -- start Cycle 1
    loop
        --  __Main_CPU_Loop__
        Draw_Pixel (X);
        Next_Pixel;

        Get (Input, Code);
        Cycle := @ + 1; -- next Cycle

        if Code = "addx" then
           Draw_Pixel (X);
           Next_Pixel;

           Get (Input, Value);
           Cycle := @ + 1; -- next Cycle
           X     := X + Value;
        end if;
        exit when End_Of_File (Input);
    end loop;

    Close (Input);

    Put_Line ("Answer:");
    Draw_CRT;
end Day10_P2;
