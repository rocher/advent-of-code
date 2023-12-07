-------------------------------------------------------------------------------
--
--  AOC202307 - Advent of Code 2023 - Day 7 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Camel_Cards is

   --------------
   -- Strength --
   --------------

   function Strength (Hand : Hands) return Strengths is
      Cards_Count : Cards_Counter := [others => 0];
      Max_Count   : Natural       := 0;
   begin
      return Result : Strengths := High_Card do
         for Card of Hand loop
            Cards_Count (Card) := @ + 1;
            Max_Count          := Natural'Max (@, Cards_Count (Card));
         end loop;

         case Max_Count is
            when 5 =>
               Result := Five_Of_A_Kind;

            when 4 =>
               Result := Four_Of_A_Kind;

            when 3 =>
               Result := Three_Of_A_Kind;
               for Count of Cards_Count loop
                  if Count = 2 then
                     Result := Full_House;
                  end if;
               end loop;

            when 2 =>
               Result := One_Pair;
               Pairs : Natural := 0;
               for Count of Cards_Count loop
                  if Count = 2 then
                     Pairs := @ + 1;
                  end if;
               end loop;
               if Pairs = 2 then
                  Result := Two_Pair;
               end if;

            when others => null;
         end case;
      end return;
   end Strength;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Plays) return Boolean is
   begin
      return Result : Boolean := False do
         if Left.Hand.Strength /= Right.Hand.Strength then
            Result := Left.Hand.Strength < Right.Hand.Strength;
         else
            for I in Hands'Range loop
               if Left.Hand (I) /= Right.Hand (I) then
                  Result := Left.Hand (I) < Right.Hand (I);
                  return;
               end if;
            end loop;
         end if;
      end return;
   end "<";

end Camel_Cards;
