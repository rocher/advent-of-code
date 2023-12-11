[![Static Badge](https://img.shields.io/badge/Advent_of_Ada-Coding_for_a_cause-darkviolet?style=for-the-badge)](https://blog.adacore.com/announcing-advent-of-ada-2023-coding-for-a-cause)
[![Static Badge](https://img.shields.io/badge/Posted_To-Forum_Ada_Lang-darkcyan?style=for-the-badge)](https://forum.ada-lang.io/t/charity-advent-of-ada-spark-2023-submissions)

[![Static Badge](https://img.shields.io/badge/AoC_2023-Day_11-blue)](https://adventofcode.com/2023/day/11)
[![Static Badge](https://img.shields.io/badge/Ada-2022-blue)](https://ada-lang.io/docs/arm)
[![Static Badge](https://img.shields.io/badge/Build_with-Alire-blue)](https://alire.ada.dev/)
![Maintained?](https://img.shields.io/badge/Maintained%3F-yes-33aa33)
![Issues](https://img.shields.io/github/issues/rocher/advent-of-code.svg?label=Issues&color=grey)
[![License](https://img.shields.io/github/license/rocher/advent-of-code.svg?label=License&color=blue)](https://github.com/rocher/advent-of-code/blob/main/LICENSE)

##
# Advent of Code 2023 - Day 11

### Cosmic Expansion

> *The researcher is trying to figure out the sum of the lengths of the
> shortest path between every pair of galaxies.*

### Part 1
[![Static Badge](https://img.shields.io/badge/read-part__1.adb-blue)](src/part_1.adb)

#### Universe expansion

If I had the opportunity to read part 2 in advance, I had implemented part 1
in the same way.

In part 1, the *real* expansion of the universe requires two arrays, the
source `Image` and the `Universe`:

```ada
   subtype Image_Range is Natural range 1 .. 140;
   subtype Universe_Range is
     Natural range Image_Range'First .. 2 * Image_Range'Last;

   type Images is array (Image_Range, Image_Range) of Character;
   type Universes is array (Universe_Range, Universe_Range) of Character;
```

Next step is to localize the galaxies and put their positions in a vector.
Finally, compute the Manhattan distance between all galaxy pairs.

### Part 2
[![Static Badge](https://img.shields.io/badge/read-part__2.adb-blue)](src/part_2.adb)

#### Much farther apart

Now the expansion of the universe adds 1.000.000 rows of columns at each empty
row or column, which is clearly impossible to do with arrays.

Instead, each empty space (row or column) is marked with an 'M' at the initial
position. The distance between galaxies is computed using the Manhattan
distance, but adding 999.999 each time a row or column marked with 'M' is
crossed.

For two galaxies located at positions $P_1$, $P_2$:

```ada
   for Y in Natural'Min (P1.Y, P2.Y) .. Natural'Max (P1.Y, P2.Y) loop
      if Image (1, Y) = 'M' then
         Answer := @ + 999_999; --  1_000_000 - the column itself
      end if;
   end loop;

   for X in Natural'Min (P1.X, P2.X) .. Natural'Max (P1.X, P2.X) loop
      if Image (X, 1) = 'M' then
         Answer := @ + 999_999; --  1_000_000 - the row itself
      end if;
   end loop;
```

##
### License
MIT (c) 2023 Francesc Rocher
