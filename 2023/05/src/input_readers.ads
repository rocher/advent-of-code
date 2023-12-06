-------------------------------------------------------------------------------
--
--  AOC202305 - Advent of Code 2023 - Day 5 - Part 1
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;             use Ada.Text_IO;
with Containers.Almanac_Maps; use Containers.Almanac_Maps;
with Containers.Seeds;        use Containers.Seeds;

package Input_Readers is

   procedure Read_Seeds (Input : File_Type; Seeds : in out Seeds_List);

   procedure Read_Maps_List
     (Input : File_Type; Maps_List : in out Almanac_Maps_List);

end Input_Readers;
