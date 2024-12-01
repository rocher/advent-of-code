[![Static Badge](https://img.shields.io/badge/Advent_of_Ada-Coding_for_a_cause-darkviolet?style=for-the-badge)](https://blog.adacore.com/announcing-advent-of-ada-2024-coding-for-a-cause)
[![Static Badge](https://img.shields.io/badge/Posted_To-Forum_Ada_Lang-darkcyan?style=for-the-badge)](https://forum.ada-lang.io/t/charity-advent-of-ada-spark-2024-submissions)

[![Static Badge](https://img.shields.io/badge/AoC_2024-Day_1-blue)](https://adventofcode.com/2024/day/1)
[![Static Badge](https://img.shields.io/badge/Ada-2022-blue)](https://ada-lang.io/docs/arm)
[![Static Badge](https://img.shields.io/badge/Build_with-Alire-blue)](https://alire.ada.dev/)
![Maintained?](https://img.shields.io/badge/Maintained%3F-yes-33aa33)
![Issues](https://img.shields.io/github/issues/rocher/advent-of-code.svg?label=Issues&color=grey)
[![License](https://img.shields.io/github/license/rocher/advent-of-code.svg?label=License&color=blue)](https://github.com/rocher/advent-of-code/blob/main/LICENSE)

##
# Advent of Code 2024 - Day 1

### Historian Hysteria

> *Maybe the lists are only off by a small amount! To find out, pair up the
> numbers and measure how far apart they are. Pair up the smallest number in
> the left list with the smallest number in the right list, then the
> second-smallest left number with the second-smallest right number, and so
> on.*

### Part 1
[![Static Badge](https://img.shields.io/badge/read-part__1.adb-blue)](src/part_1.adb)

#### Pair sorted lists

The main idea is to sort both lists, `Left_List` and `Right_List`, and
compute the absolute value of the difference of each pair of elements.

```ada
declare
   Left  : Number_Lists.Cursor := Left_List.First;
   Right : Number_Lists.Cursor := Right_List.First;
begin
   loop
      Answer := @ + abs (Left.Element - Right.Element);
      Left := Left.Next;
      Right := Right.Next;
      exit when not Left.Has_Element;
   end loop;
end;
```

### Part 2
[![Static Badge](https://img.shields.io/badge/read-part__2.adb-blue)](src/part_2.adb)

#### Count element appearances

Sort both `Left_List` and `Right_List`, and then traverse `Left_List`
(skipping repeated elements) to count how many times appears each element in
`Rigt_List`.

```ada
declare
   Left  : Number_Lists.Cursor := Left_List.First;
   Right : Number_Lists.Cursor := Number_Lists.No_Element;
   Times : Natural := 0;
begin
   loop
      --  Count how many times Left.Element appears in Right_List
      Times := 0;
      Right := Right_List.Find (Left.Element);
      if Right /= Number_Lists.No_Element then
         loop
            Times := @ + 1;
            Right := Right.Next;
            exit when Right.Element /= Left.Element;
         end loop;
      end if;
      Answer := @ + (Left.Element * Times);

      --  Skip repeated numbers in Left_List
      declare
         Current_Element : constant Natural := Left.Element;
      begin
         loop
            Left := Left.Next;
            exit when
              Left = Number_Lists.No_Element
              or else Left.Element /= Current_Element;
         end loop;
      end;

      exit when not Left.Has_Element;
   end loop;
end;
```

##
### License
MIT (c) 2024 Francesc Rocher
