[![Static Badge](https://img.shields.io/badge/Advent_of_Ada-Coding_for_a_cause-red?style=for-the-badge&labelColor=black&color=darkviolet)](https://blog.adacore.com/announcing-advent-of-ada-2023-coding-for-a-cause)
[![Static Badge](https://img.shields.io/badge/Posted_To-Forum_Ada_Lang-red?style=for-the-badge&labelColor=black&color=darkcyan)](https://forum.ada-lang.io/t/charity-advent-of-ada-spark-2023-submissions)

[![Static Badge](https://img.shields.io/badge/AoC_2023-Day_2-blue?labelColor=black)](https://adventofcode.com/2023/day/2)
[![Static Badge](https://img.shields.io/badge/Ada-2022-blue?labelColor=black)](https://ada-lang.io/docs/arm)
[![Static Badge](https://img.shields.io/badge/Build_with-Alire-blue?labelColor=black)](https://alire.ada.dev/)
![Maintained?](https://img.shields.io/badge/Maintained%3F-yes-red.svg?labelColor=black&color=33aa33)
![Issues](https://img.shields.io/github/issues/rocher/advent-of-code.svg?labelColor=black&color=grey)
[![License](https://img.shields.io/github/license/rocher/advent-of-code.svg?labelColor=black&color=blue)](https://github.com/rocher/advent-of-code/blob/main/LICENSE)

##
# Advent of Code 2023 - Day 2

### Cube Conundrum

> *As you walk, the Elf shows you a small bag and some cubes which are either
> red, green, or blue. Each time you play this game, he will hide a secret
> number of cubes of each color in the bag, and your goal is to figure out
> information about the number of cubes.*

### Part 1
[![Static Badge](https://img.shields.io/badge/part__1.adb-blue?label=read&labelColor=black)](src/part_1.adb)

#### Process file lines
This time the lines must be parsed to extract numbers and colors `red`,
`green` or `blue`. There are separators, `,` and `;`, and game identifiers
that are not required to be parsed at all.

This time each line is read with and `Unbounded_String`:

```ada
   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   with Ada.Strings.Unbounded.Text_IO;

   --  · · ·

   Input.Open (In_File, Filename);
   loop
      declare
         Line : Parse_String_Type :=
           (0, Ada.Strings.Unbounded.Text_IO.Get_Line (Input));
         Token : Unbounded_String;
      begin
         Parsing_Line :
         loop
            Token := Next_Token (Line);
            exit Parsing_Line when Token = Null_Unbounded_String;
            --  process token
         end loop Parsing_Line;
      end;
      exit when Input.End_Of_File;
   end loop;
   Input.Close;
```

`Parse_String_Type` is a record with a cursor and the string. The function
`Nex_Token (Line)` return the next token in the line (substring surrounded by
spaces, BOL or EOL). It returns `Null_Unbounded_String` if there are no more
tokens.

I'm pretty sure that this pattern will be useful for future days.

### Part 2
[![Static Badge](https://img.shields.io/badge/part__2.adb-blue?label=read&labelColor=black)](src/part_2.adb)

#### Compute maximum

This part is very similar to part 1. In this case it es necessary to compute
the *power* of a game (see description in AoC) with the minimum number of
cubes that make the game possible which, in fact, are the maximum number of
cubes of all games in a line:

```ada
   if Color = "red" then
      Min_Red := Natural'Max (@, Number);
   elsif Color = "green" then
      Min_Green := Natural'Max (@, Number);
   else
      Min_Blue := Natural'Max (@, Number);
   end if;

   --  · · ·

   Answer  := @ + (Min_Red * Min_Green * Min_Blue);
```

##
### License
MIT (c) 2023 Francesc Rocher
