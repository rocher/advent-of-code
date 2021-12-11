- [PART 1](#sec-1)
  - [Statement](#sec-1-1)
  - [Solution](#sec-1-2)
  - [Implementation](#sec-1-3)
    - [Declarations](#sec-1-3-1)
    - [Process](#sec-1-3-2)
    - [Result](#sec-1-3-3)
    - [Compilation unit and evaluation](#sec-1-3-4)
- [PART 2](#sec-2)
- [REFERENCES](#sec-3)


# PART 1<a id="sec-1"></a>

## Statement<a id="sec-1-1"></a>

<div class="html" id="orgf3f6285">
<p>
&lt;details&gt;
&lt;summary&gt;
  &lt;i&gt;click to expand&lt;/i&gt;
&lt;/summary&gt;
</p>

</div>

The sea floor is getting steeper. Maybe the sleigh keys got carried this way?

A massive school of glowing *lanternfish* swims past. They must spawn quickly to reach such large numbers - maybe **exponentially** quickly? You should model their growth rate to be sure.

Although you know nothing about this specific species of lanternfish, you make some guesses about their attributes. Surely, each lanternfish creates a new lanternfish once every 7 days.

However, this process isn't necessarily synchronized between every lanternfish - one lanternfish might have 2 days left until it creates another lanternfish, while another might have 4. So, you can model each fish as a single number that represents **the number of days until it creates a new lanternfish**.

Furthermore, you reason, a **new** lanternfish would surely need slightly longer before it's capable of producing more lanternfish: two more days for its first cycle.

So, suppose you have a lanternfish with an internal timer value of 3:

-   After one day, its internal timer would become `2`.
-   After another day, its internal timer would become `1`.
-   After another day, its internal timer would become `0`.
-   After another day, its internal timer would reset to `6`, and it would create a new lanternfish with an internal timer of `8`.
-   After another day, the first lanternfish would have an internal timer of `5`, and the second lanternfish would have an internal timer of `7`.

A lanternfish that creates a new fish resets its timer to `6`, **not 7** (because 0 is included as a valid timer value). The new lanternfish starts with an internal timer of `8` and does not start counting down until the next day.

Realizing what you're trying to do, the submarine automatically produces a list of the ages of several hundred nearby lanternfish (your puzzle input). For example, suppose you were given the following list:

    3,4,3,1,2

This list means that the first fish has an internal timer of `3`, the second fish has an internal timer of `4`, and so on until the fifth fish, which has an internal timer of `2`. Simulating these fish over several days would proceed as follows:

    Initial state: 3,4,3,1,2
    After  1 day:  2,3,2,0,1
    After  2 days: 1,2,1,6,0,8
    After  3 days: 0,1,0,5,6,7,8
    After  4 days: 6,0,6,4,5,6,7,8,8
    After  5 days: 5,6,5,3,4,5,6,7,7,8
    After  6 days: 4,5,4,2,3,4,5,6,6,7
    After  7 days: 3,4,3,1,2,3,4,5,5,6
    After  8 days: 2,3,2,0,1,2,3,4,4,5
    After  9 days: 1,2,1,6,0,1,2,3,3,4,8
    After 10 days: 0,1,0,5,6,0,1,2,2,3,7,8
    After 11 days: 6,0,6,4,5,6,0,1,1,2,6,7,8,8,8
    After 12 days: 5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8
    After 13 days: 4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8
    After 14 days: 3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8
    After 15 days: 2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7
    After 16 days: 1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8
    After 17 days: 0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8
    After 18 days: 6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8

Each day, a `0` becomes a `6` and adds a new `8` to the end of the list, while each other number decreases by 1 if it was present at the start of the day.

In this example, after 18 days, there are a total of `26` fish. After 80 days, there would be a total of **5934**.

Find a way to simulate lanternfish. **How many lanternfish would there be after 80 days?**

<div class="html" id="orgbfb6755">
<p>
&lt;/details&gt;
</p>

</div>

## Solution<a id="sec-1-2"></a>

This part is solved using *brute force*. That is, simulating the 80 days of lanternfish generations using the input population.

## Implementation<a id="sec-1-3"></a>

### Declarations<a id="sec-1-3-1"></a>

The timer type associated to each lanternfish is:

```ada

--  __Types__
subtype Timer_Type is Natural range 0 .. 8;

```

This type is the element of the doubly linked list used to store the lanternfish generations:

```ada

--  __Packages__
 package Lanterfish_School is new Doubly_Linked_Lists (Timer_Type, "=");
 package Timer_IO is new Ada.Text_IO.Integer_IO (Timer_Type);

```

Variables

```ada

--  __Veriables__
Input : File_Type;
Comma : Character;

 Timer  : Timer_Type;
 School : Lanterfish_School.List;
 Fish   : Lanterfish_School.Cursor;
 Resets : Natural := 0;

```

### Process<a id="sec-1-3-2"></a>

```ada

-- __Read_Timers__
Timer_IO.Get (Input, Timer);
loop
   School.Append (Timer);
   exit when End_Of_File (Input);
   Get (Input, Comma);
   Timer_IO.Get (Input, Timer);
end loop;

```

```ada

--  __Simulate_80_Days
for Day in 1 .. 80 loop
   Fish := School.First;
   loop
      --  decrement timers or current fishes
      Timer := Lanterfish_School.Element (Fish);
      if Timer = 0 then
         Resets := Resets + 1;
         School.Replace_Element (Fish, 6);
      else
         School.Replace_Element (Fish, Timer - 1);
      end if;

      Lanterfish_School.Next (Fish);
      exit when not Lanterfish_School.Has_Element (Fish);
   end loop;

   --  add new born fishes
   School.Append (8, Count_Type (Resets));
   Resets := 0;
end loop;

```

### Result<a id="sec-1-3-3"></a>

```ada

--  __Result__
Put_Line ("Answer:" & School.Length'Image);

```

### Compilation unit and evaluation<a id="sec-1-3-4"></a>

The compilation unit for this program is obtained with the composition of the source code blocks shown here (evaluation of the program appears below):

```ada
with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;

procedure Day06_P1 is

   --  __Types__
   subtype Timer_Type is Natural range 0 .. 8;


   --  __Packages__
    package Lanterfish_School is new Doubly_Linked_Lists (Timer_Type, "=");
    package Timer_IO is new Ada.Text_IO.Integer_IO (Timer_Type);


   --  __Veriables__
   Input : File_Type;
   Comma : Character;

    Timer  : Timer_Type;
    School : Lanterfish_School.List;
    Fish   : Lanterfish_School.Cursor;
    Resets : Natural := 0;

begin
   Open (Input, In_File, "__Path__" & "input");

     -- __Read_Timers__
     Timer_IO.Get (Input, Timer);
     loop
        School.Append (Timer);
        exit when End_Of_File (Input);
        Get (Input, Comma);
        Timer_IO.Get (Input, Timer);
     end loop;

   Close (Input);

   --  __Simulate_80_Days
   for Day in 1 .. 80 loop
      Fish := School.First;
      loop
         --  decrement timers or current fishes
         Timer := Lanterfish_School.Element (Fish);
         if Timer = 0 then
            Resets := Resets + 1;
            School.Replace_Element (Fish, 6);
         else
            School.Replace_Element (Fish, Timer - 1);
         end if;

         Lanterfish_School.Next (Fish);
         exit when not Lanterfish_School.Has_Element (Fish);
      end loop;

      --  add new born fishes
      School.Append (8, Count_Type (Resets));
      Resets := 0;
   end loop;


   --  __Result__
   Put_Line ("Answer:" & School.Length'Image);

end Day06_P1;
```

# PART 2<a id="sec-2"></a>

# REFERENCES<a id="sec-3"></a>

-   source: [Advent of Code 2021, day 6](https://adventofcode.com/2021/day/6)
-   tangled source code files:
    -   <src/day06_p1.adb>
    -   <src/day06_p2.adb>
-   input file: <input>
-   Ada/SPARK support for Org-Babel: [ob-ada-spark](https://github.com/rocher/ob-ada-spark)
