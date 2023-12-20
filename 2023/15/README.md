[![Static Badge](https://img.shields.io/badge/Advent_of_Ada-Coding_for_a_cause-darkviolet?style=for-the-badge)](https://blog.adacore.com/announcing-advent-of-ada-2023-coding-for-a-cause)
[![Static Badge](https://img.shields.io/badge/Posted_To-Forum_Ada_Lang-darkcyan?style=for-the-badge)](https://forum.ada-lang.io/t/charity-advent-of-ada-spark-2023-submissions)

[![Static Badge](https://img.shields.io/badge/AoC_2023-Day_15-blue)](https://adventofcode.com/2023/day/15)
[![Static Badge](https://img.shields.io/badge/Ada-2022-blue)](https://ada-lang.io/docs/arm)
[![Static Badge](https://img.shields.io/badge/Build_with-Alire-blue)](https://alire.ada.dev/)
![Maintained?](https://img.shields.io/badge/Maintained%3F-yes-33aa33)
![Issues](https://img.shields.io/github/issues/rocher/advent-of-code.svg?label=Issues&color=grey)
[![License](https://img.shields.io/github/license/rocher/advent-of-code.svg?label=License&color=blue)](https://github.com/rocher/advent-of-code/blob/main/LICENSE)

##
# Advent of Code 2023 - Day 15

### Lens Library

> *The newly-focused parabolic reflector dish is sending all of the collected
> light to a point on the side of yet another mountain*

### Part 1
[![Static Badge](https://img.shields.io/badge/read-part__1.adb-blue)](src/part_1.adb)

#### Compute hash

No problem to compute the hash:

```ada
   function Hash (S : String) return Natural is
   begin
      return Result : Natural := 0 do
         for C of S loop
            Result := @ + Character'Pos (C);
            Result := (@ * 17) mod 256;
         end loop;
      end return;
   end Hash;
```

### Part 2
[![Static Badge](https://img.shields.io/badge/read-part__2.adb-blue)](src/part_2.adb)

#### Compute focusing power

For the boxes of lens, doubly linked lists are very convenient to store and
manipulate several lens:

```ada
   type Lens_Type is record
      Label        : Ada.Strings.Unbounded.Unbounded_String;
      Focal_Length : Natural;
   end record;

   package Lens_Lists is new Doubly_Linked_Lists (Lens_Type);
   use all type Lens_Lists.Cursor;
   subtype Lens_List is Lens_Lists.List;

   Box : array (0 .. 255) of Lens_List := [others => Lens_Lists.Empty_List];
```

##
### License
MIT (c) 2023 Francesc Rocher
