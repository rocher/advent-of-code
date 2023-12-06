[![Static Badge](https://img.shields.io/badge/Advent_of_Ada-Coding_for_a_cause-darkviolet?style=for-the-badge)](https://blog.adacore.com/announcing-advent-of-ada-2023-coding-for-a-cause)
[![Static Badge](https://img.shields.io/badge/Posted_To-Forum_Ada_Lang-darkcyan?style=for-the-badge)](https://forum.ada-lang.io/t/charity-advent-of-ada-spark-2023-submissions)

[![Static Badge](https://img.shields.io/badge/AoC_2023-Day_5-blue)](https://adventofcode.com/2023/day/5)
[![Static Badge](https://img.shields.io/badge/Ada-2022-blue)](https://ada-lang.io/docs/arm)
[![Static Badge](https://img.shields.io/badge/Build_with-Alire-blue)](https://alire.ada.dev/)
![Maintained?](https://img.shields.io/badge/Maintained%3F-yes-33aa33)
![Issues](https://img.shields.io/github/issues/rocher/advent-of-code.svg?label=Issues&color=grey)
[![License](https://img.shields.io/github/license/rocher/advent-of-code.svg?label=License&color=blue)](https://github.com/rocher/advent-of-code/blob/main/LICENSE)

##
# Advent of Code 2023 - Day 5

### If You Give A Seed A Fertilizer

> *The gardener and his team want to get started as soon as possible, so
> they'd like to know the closest location that needs a seed. Using these
> maps, find the lowest location number that corresponds to any of the initial
> seeds.*

### Part 1
[![Static Badge](https://img.shields.io/badge/read-part__1.adb-blue)](src/part_1.adb)

#### Big function composition

This part is simply a composition of functions for a given seed: you have to
got through all the maps:

  * seed-to-soil
  * soil-to-fertilizer
  * ..
  * humidity-to-location

If each map is represented by a function $f_i$, the final location for a seed
$s$ looks like

  $$f_7( \ldots (f_2(f_1(s))) \ldots )$$

The complex part resides in parsing the input file, storing these maps in a
convenient way and providing functions to operate on them to easily apply such
composition.

Solution implemented here (briefly):

  1. `Almanac_Map` is a record to hold an almanac map, 1 line of the file:
   ```ada
      type Almanac_Map is record
         Source      : Long_Integer;
         Destination : Long_Integer;
         Length      : Long_Integer;
      end record;
   ```

  2. Each map consist in fact in a list of maps, an `Almanac_Maps_List`:
  ```ada
     package Almanac_Maps_Lists is new Doubly_Linked_Lists (Element_Type => Almanac_Map);
     subtype Almanac_Maps_List is Almanac_Maps_LIsts.LIst;
  ```

  3. The whole almanac is an array of `Almanac_Maps_List`; there a total of
     `Number_Of_Maps`, so:
  ```ada
     type Almanacs_Array is array (1 .. Number_Of_Maps) of Almanac_Maps_List;
  ```

With a pair of functions that return the destination of a source in an
`Almanac_Map` and in an `Almanac_Maps_List`, the implementation of part 1 must
traverse the list of seeds to find the minimum:

```ada
   declare
      Destination : Long_Integer;
      Cursor      : Seeds_Lists.Cursor := Seeds.First;
   begin
      loop
         --  Destination = seed
         Destination := Cursor.Element;
         for I in Almanac'Range loop
            Destination := Get_Destination (Almanac (I), Destination);
         end loop;
         --  Destination = location

         Answer := Long_Integer'Min (@, Destination);

         Cursor := Cursor.Next;
         exit when not Cursor.Has_Element;
      end loop;
```

### Part 2
[![Static Badge](https://img.shields.io/badge/read-part__2.adb-blue)](src/part_2.adb)

#### Pure brute force

I had the temptation to implement this part using *brute force* ... and I did!

But I used *parallel brute force*, consisting in porting the implementation of
part 1 to a `Worker_Task` and running them in parallel. In my case, the input
file has 10 seeds intervals, and I have a 16-core CPU. Couldn't it be more
easy!?

```ada
   declare
      Start, Length : Long_Integer;
      Cursor        : Seeds_Lists.Cursor := Seeds.First;
      CPU           : CPU_Range          := CPU_Range'First;
      Worker        : array (1 .. Seeds_Intervals) of Worker_Task;
   begin
      for I in 1 .. Seeds_Intervals loop
         Start  := Cursor.Element;
         Cursor := Cursor.Next;
         Length := Cursor.Element;
         Cursor := Cursor.Next;

         Set_CPU (CPU, Worker (I)'Identity);
         Worker (I).Initialize
           (I, Almanac'Unrestricted_Access, Start, Length,
            Results'Unrestricted_Access);

         exit when not Cursor.Has_Element;

         if CPU = CPU_Range'Last then
            CPU := CPU_Range'First;
         else
            CPU := CPU + 1;
         end if;
      end loop;

   --  wait for al workers to finish

   for Result of Results loop
      Answer := Long_Integer'Min (@, Result);
   end loop;
```

##
### License
MIT (c) 2023 Francesc Rocher
