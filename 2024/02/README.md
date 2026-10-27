[![Static Badge](https://img.shields.io/badge/Advent_of_Ada-Coding_for_a_cause-darkviolet?style=for-the-badge)](https://blog.adacore.com/announcing-advent-of-ada-2024-coding-for-a-cause)
[![Static Badge](https://img.shields.io/badge/Posted_To-Forum_Ada_Lang-darkcyan?style=for-the-badge)](https://forum.ada-lang.io/t/charity-advent-of-ada-spark-2024-submissions)

[![Static Badge](https://img.shields.io/badge/AoC_2024-Day_1-blue)](https://adventofcode.com/2024/day/1)
[![Static Badge](https://img.shields.io/badge/Ada-2022-blue)](https://ada-lang.io/docs/arm)
[![Static Badge](https://img.shields.io/badge/Build_with-Alire-blue)](https://alire.ada.dev/)
![Maintained?](https://img.shields.io/badge/Maintained%3F-yes-33aa33)
![Issues](https://img.shields.io/github/issues/rocher/advent-of-code.svg?label=Issues&color=grey)
[![License](https://img.shields.io/github/license/rocher/advent-of-code.svg?label=License&color=blue)](https://github.com/rocher/advent-of-code/blob/main/LICENSE)

##
# Advent of Code 2024 - Day 2

### Red-Nose Reports

> *The engineers are trying to figure out which reports are safe. The
> Red-Nosed reactor safety systems can only tolerate levels that are either
> gradually increasing or gradually decreasing.*

### Part 1
[![Static Badge](https://img.shields.io/badge/read-part__1.adb-blue)](src/part_1.adb)

#### Counting safe reports

Traverse the list of reports and examine level by level whether it is safe or
not.

### Part 2
[![Static Badge](https://img.shields.io/badge/read-part__2.adb-blue)](src/part_2.adb)

#### Problem Dampener

Unsafe reports must be checked if removing an element they become safe:

```ada
if Is_Safe (Report) then
   Answer := @ + 1;
else
   for I in 1 .. Report.Length loop
      if Is_Safe (Remove_Element (I, Report)) then
         Answer := @ + 1;
         exit;
      end if;
   end loop;
end if;
```

##
### License
MIT (c) 2024 Francesc Rocher
