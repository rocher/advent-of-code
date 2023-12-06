-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Containers.Almanac_Maps is

   function Get_Destination
     (List : Almanac_Maps_List; Source : Long_Integer) return Long_Integer
   is
      Map     : Almanac_Map;
      Cursor  : Almanac_Maps_Lists.Cursor := List.First;
      Has_Map : Boolean                   := False;
   begin
      return Destination : Long_Integer := Source do
         loop
            Map         := Cursor.Element;
            Destination := Map.Get_Destination (Source, Has_Map);
            if Has_Map then
               return;
            end if;
            Cursor := Cursor.Next;
            exit when not Cursor.Has_Element;
         end loop;
      end return;
   end Get_Destination;

end Containers.Almanac_Maps;
