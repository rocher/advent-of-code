[![Static Badge](https://img.shields.io/badge/Advent_of_Ada-Coding_for_a_cause-darkviolet?style=for-the-badge)](https://blog.adacore.com/announcing-advent-of-ada-2023-coding-for-a-cause)
[![Static Badge](https://img.shields.io/badge/Posted_To-Forum_Ada_Lang-darkcyan?style=for-the-badge)](https://forum.ada-lang.io/t/charity-advent-of-ada-spark-2023-submissions)

[![Static Badge](https://img.shields.io/badge/AoC_2023-Day_4-blue)](https://adventofcode.com/2023/day/4)
[![Static Badge](https://img.shields.io/badge/Ada-2022-blue)](https://ada-lang.io/docs/arm)
[![Static Badge](https://img.shields.io/badge/Build_with-Alire-blue)](https://alire.ada.dev/)
![Maintained?](https://img.shields.io/badge/Maintained%3F-yes-33aa33)
![Issues](https://img.shields.io/github/issues/rocher/advent-of-code.svg?label=Issues&color=grey)
[![License](https://img.shields.io/github/license/rocher/advent-of-code.svg?label=License&color=blue)](https://github.com/rocher/advent-of-code/blob/main/LICENSE)

##
# Advent of Code 2023 - Day 4

### Scratchcards

> *...  you have to figure out which of the numbers you have appear in the
> list of winning numbers. The first match makes the card worth one point and
> each match after the first doubles the point value of that card.*

### Part 1
[![Static Badge](https://img.shields.io/badge/read-part__1.adb-blue)](src/part_1.adb)

#### Matching numbers

The algorithm is trivial: just check ifa number is a winning number. If so,
the value of the card doubles.

The format of the input file is so easy that there is no need of parsing
complexities:

```txt
Card   1: 18 39  5 97 33 74 70 35 40 72 | 62 23 33 94 18  5 91 74 86 88 82 72 ..
Card   2: 58 50 13 61 80 48 99 86 45 31 | 61 32 19  6 72 31 52 79 93 45 85 67 ..
..
Card  99: 31 47 15 27 68 72 90 21 29 55 | 64 46 83 79 35 90  5 58 34 77 59 75 ..
Card 100: 47 79 18 44 28 25 86 43 39 95 | 75 92 95 88 32 98 78 36 33 15 51 79 ..
..
Card 199: 74 30 60 87 46 21 69 51 28 96 | 92 64 32 66 65 54 29 85 84  2  6  3 ..
```

The point is to define the card and winning numbers appropriately:

```ada
      declare
         Line            : constant String           := Input.Get_Line;
         Card_Numbers    : constant String           := Line (42 .. 116);
         Winning_Numbers : constant String (1 .. 30) := Line (10 .. 39);
```

Then, using the `Index` function defined in `Strings.Fixed`,

```ada
   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
```

the only thing to do is to compare all winning numbers with the card numbers
and accumulate the number of `Matches`:

```ada
   for I in 1 .. 10 loop
      J : constant Natural := ((I - 1) * 3) + 1;
      if Index (Card_Numbers, Winning_Numbers (J .. J + 2)) > 0 then
         Matches := Matches + 1;
      end if;
   end loop;
```

Note that the comparison is taking three characters, including the initial
space. Otherwise, ` 4` would match ` 75 64 86 42 13 98` and that's incorrect.
Taking three characters, `  4` does not match that string.

### Part 2
[![Static Badge](https://img.shields.io/badge/read-part__2.adb-blue)](src/part_2.adb)

#### Winning more scratchpads

This part is more simple than it seems. Counting how many winning numbers
there are in a card is exactly the same, but now these matches must become
_copies_ of following cards (as many as matches).

Every card contains ten winning numbers, so it's not possible to win more than
ten copies with a card. The array to hold the copies is

```ada
   Copies : array (0 .. 10) of Natural := [0 ..  10 => 0];
```

Each card must add to the answer the number of instances, the original plus
the copies:

```ada
   Copies (0) := @ + 1;  --  instances = current card + copies won
   Answer     := @ + Copies (0);
```

Once `Matches` contains the count of matches of the current card, it is
necessary to _clone_ the next cards. _Every instance_ is cloning the following
cards, so there will be as many copies as instances of the current card:

```ada
   if Matches > 0 then
      for I in 1 .. Matches loop
         Copies (I) := @ + Copies (0);
      end loop;
   end if;
```

Finally, prepare the array for the next card:

```ada
   Copies (0 .. 9) := Copies (1 .. 10);
   Copies (10)     := 0;
```

##
### License
MIT (c) 2023 Francesc Rocher
