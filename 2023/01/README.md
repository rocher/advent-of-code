[![Static Badge](https://img.shields.io/badge/Advent_of_Ada-Coding_for_a_cause-red?style=for-the-badge&labelColor=black&color=darkviolet)](https://blog.adacore.com/announcing-advent-of-ada-2023-coding-for-a-cause)
[![Static Badge](https://img.shields.io/badge/Posted_To-Forum_Ada_Lang-red?style=for-the-badge&labelColor=black&color=darkcyan)](https://forum.ada-lang.io/t/charity-advent-of-ada-spark-2023-submissions/562)

[![Static Badge](https://img.shields.io/badge/AoC_2023-Day_1-blue?labelColor=black)](https://adventofcode.com/2023/day/1)
[![Static Badge](https://img.shields.io/badge/Build_with-Alire-blue?labelColor=black)](https://alire.ada.dev/)
![Maintained?](https://img.shields.io/badge/Maintained%3F-yes-red.svg?labelColor=black&color=33aa33)
![Issues](https://img.shields.io/github/issues/rocher/advent-of-code.svg?labelColor=black&color=grey)
[![License](https://img.shields.io/github/license/rocher/advent-of-code.svg?labelColor=black&color=blue)](https://github.com/rocher/advent-of-code/blob/main/LICENSE)

##
# Advent of Code 2023 - Day 1

### Trebuchet!?

> *Something is wrong with global snow production, and you've been selected to
> take a look. The Elves have even given you a map; on it, they've used stars
> to mark the top fifty locations that are likely to be having problems.*

### Part 1 [![Static Badge](https://img.shields.io/badge/View-Part_1-blue?labelColor=black)](src/part_1.adb)

#### Process file lines
Input file must be read every day, and every day the format is different.
Read and process each line using `String` type:

```ada
Input.Open (In_File, "input.txt");
loop
   declare
      Line : constant String := Input.Get_Line;
   begin
      -- process Line
   end;
   exit when Input.End_Of_File;
end loop;
Input.Close;
```

### [Part 2](src/part_2.adb)

#### Convert strings to numbers
Part 2 requires to  convert strings `one`, `two`, .. `nine` to the respective
number:

```ada
case Text is
   when "one"    => Put_Digit (1);
   when "two"    => Put_Digit (2);
   when "three"  => Put_Digit (3);
   when "four"   => Put_Digit (4);
   when "five"   => Put_Digit (5);
   when "six"    => Put_Digit (6);
   when "seven"  => Put_Digit (7);
   when "eight"  => Put_Digit (8);
   when "nine"   => Put_Digit (9);
   when "others" => null;
end case;
```

##
### License
MIT (c) 2023 Francesc Rocher
