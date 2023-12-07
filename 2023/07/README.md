[![Static Badge](https://img.shields.io/badge/Advent_of_Ada-Coding_for_a_cause-darkviolet?style=for-the-badge)](https://blog.adacore.com/announcing-advent-of-ada-2023-coding-for-a-cause)
[![Static Badge](https://img.shields.io/badge/Posted_To-Forum_Ada_Lang-darkcyan?style=for-the-badge)](https://forum.ada-lang.io/t/charity-advent-of-ada-spark-2023-submissions)

[![Static Badge](https://img.shields.io/badge/AoC_2023-Day_7-blue)](https://adventofcode.com/2023/day/7)
[![Static Badge](https://img.shields.io/badge/Ada-2022-blue)](https://ada-lang.io/docs/arm)
[![Static Badge](https://img.shields.io/badge/Build_with-Alire-blue)](https://alire.ada.dev/)
![Maintained?](https://img.shields.io/badge/Maintained%3F-yes-33aa33)
![Issues](https://img.shields.io/github/issues/rocher/advent-of-code.svg?label=Issues&color=grey)
[![License](https://img.shields.io/github/license/rocher/advent-of-code.svg?label=License&color=blue)](https://github.com/rocher/advent-of-code/blob/main/LICENSE)

##
# Advent of Code 2023 - Day 7

### Camel Cards

> *Because the journey will take a few days, she offers to teach you the game
> of Camel Cards. Camel Cards is sort of similar to poker except it's designed
> to be easier to play while riding a camel.*

### Part 1
[![Static Badge](https://img.shields.io/badge/read-part__1.adb-blue)](src/part_1.adb)

#### Abstract data types and operators

This problem requires using a data type that provides the operator $\lt$ to
sort the hands according to its strength:

```ada
   type Cards is ('2', '3', '4', '5', '6', '7', '8', '9', J, T, Q, K, A);

   type Strengths is
     (High_Card, One_Pair, Two_Pair, Three_Of_A_Kind, Full_House,
      Four_Of_A_Kind, Five_Of_A_Kind);
```

Type `Cards` is an enumeration, but enumeration labels cannot start or be
single number, that's why from 2 to 9 the label are characters `'2'` to `'9'`.
This is distinction is important when using the package

```ada
   package Enum_IO is new Ada.Text_IO.Enumeration_IO (Cards);
```

In the input file, each hand comes with its bid, so each play is:

```ada
   type Plays is record
      Hand : Hands;
      Bid  : Natural;
   end record;
```

Finally, to store the list of all hands:

```ada
   package Hands_Package is new Doubly_Linked_Lists (Plays);
   subtype Hands_Lists is Hands_Package.List;
```

Two important functions to solve the problems are:

```ada
   function Strength (Hand : Hands) return Strengths;
   function "<" (Left, Right : Plays) return Boolean;
```

The function `Strength` is used by the second to implement the sorting rules
of the problem.

#### Reading hands

As mentioned before, when reading the list of hands with the package
`Enumeration_IO`, it's important to treat character enumeration labels in a
different way:

```ada
   for I in Hands'Range loop
      if Line (I) in '2' .. '9' then
         Enum_IO.Get ("'" & Line (I .. I) & "'", Play.Hand (I), Last);
      else
         Enum_IO.Get (Line (I .. I), Play.Hand (I), Last);
      end if;
   end loop;
```

In case of reading a character enumeration label, it must be limited with
single quotes. Didn't know this particularity, but makes sense to *prepare
exactly the same label as it appear in the enumeration definition*.

### Part 2
[![Static Badge](https://img.shields.io/badge/read-part__2.adb-blue)](src/part_2.adb)

#### Joker Cards

The second part is almost identical to the first one, with two slightly
variations,

  1. The weight of card `J`:
   ```ada
      type Cards is (J, '2', '3', '4', '5', '6', '7', '8', '9', T, Q, K, A);
   ```
  2. The implementation of the `Strength` function to take advantage of the
     wildcards `J` in a hand.

Both changes are implemented in the package `Joker_Cards`.

##
### License
MIT (c) 2023 Francesc Rocher
