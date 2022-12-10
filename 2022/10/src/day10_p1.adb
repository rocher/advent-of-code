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

procedure Day10_P1 is
   --  __CPU_Model__
   X     : Integer := 1;
   Cycle : Positive := 1;

   Code  : String (1 .. 4);
   Value : Integer;

   Input : File_Type;
   --  __Cycle_Points_And_Σ_Strength__
   Cycle_Point : array (1 .. 6) of Positive := (20, 60, 100, 140, 180, 220);

   P          : Positive := 1; -- index of Cycle_Point
   Σ_Strength : Integer  := 0;

begin
    Open (Input, In_File, "/home/ada/advent-of-code/public/2022/10/" & "input.txt");

    Execution:
    loop
        --  __CPU_Instruction_Loop__
        Get (Input, Code);

        if Code = "addx" then
           Cycle := @ + 1;
           Get (Input, Value);
        else
           Value := 0;
        end if;

        --  __Check_Cycle_Point__
        if Cycle_Point (P) <= Cycle then  -- (1)
            Σ_Strength := @ + (X * Cycle_Point (P));

            P := P + 1;
            if P > Cycle_Point'Last then
              -- signal strength computed at all cycle points
              -- no need to continue the program execution
              exit Execution;
            end if;
        end if;

        Cycle := @ + 1;
        X     := X + Value;
        exit when End_Of_File (Input);
    end loop Execution;

    Close (Input);

    Put_Line ("Answer:" & Σ_Strength'Image);
end Day10_P1;
