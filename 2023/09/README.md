[![Static Badge](https://img.shields.io/badge/Advent_of_Ada-Coding_for_a_cause-darkviolet?style=for-the-badge)](https://blog.adacore.com/announcing-advent-of-ada-2023-coding-for-a-cause)
[![Static Badge](https://img.shields.io/badge/Posted_To-Forum_Ada_Lang-darkcyan?style=for-the-badge)](https://forum.ada-lang.io/t/charity-advent-of-ada-spark-2023-submissions)

[![Static Badge](https://img.shields.io/badge/AoC_2023-Day_9-blue)](https://adventofcode.com/2023/day/9)
[![Static Badge](https://img.shields.io/badge/Ada-2022-blue)](https://ada-lang.io/docs/arm)
[![Static Badge](https://img.shields.io/badge/Build_with-Alire-blue)](https://alire.ada.dev/)
![Maintained?](https://img.shields.io/badge/Maintained%3F-yes-33aa33)
![Issues](https://img.shields.io/github/issues/rocher/advent-of-code.svg?label=Issues&color=grey)
[![License](https://img.shields.io/github/license/rocher/advent-of-code.svg?label=License&color=blue)](https://github.com/rocher/advent-of-code/blob/main/LICENSE)

##
# Advent of Code 2023 - Day 9

### Mirage Maintenance

> *To best protect the oasis, your environmental report should include a
> prediction of the next value in each history.*

### Part 1
[![Static Badge](https://img.shields.io/badge/read-part__1.adb-blue)](src/part_1.adb)

#### 2D Vectors

The way a vector of vectors is made is:

```ada
   subtype Index_Type is Natural;

   package Sensor_Data_Package is new Vectors
     (Index_Type => Index_Type, Element_Type => Integer);

   subtype Sensor_Data is Sensor_Data_Package.Vector;

   function "=" (Left, Right : Sensor_Data) return Boolean is
     (Left.Length = Right.Length
      and then
      (for all I in Left.First_Index .. Left.Last_Index =>
         Left (I) = Right (I)));

   package History_Package is new Vectors
     (Index_Type => Index_Type, Element_Type => Sensor_Data);

   subtype History_Vector is History_Package.Vector;
```

That way, the object `History : History_Vector` holds all the data and can
grown from both sides with `Prepend` and `Append` operations, respectively.

#### Read input data

Reading vectors using the package `Ada.Integer_Text_IO` is as easy as:

```ada
   declare
      Line   : constant String := Input.Get_Line;
      Last   : Natural         := 0;
      Number : Integer;

      History : History_Vector;
      Index   : Index_Type;
   begin
      History.Append (New_Item => [], Count => 1);
      Index := History.First_Index;
      loop
         Last := Last + 1;
         Int_IO.Get (Line (Last .. Line'Last), Number, Last);
         History (Index).Append (Number);
         exit when Last >= Line'Last;
      end loop;

      -- << Compute_Differences >>
      -- << Compute_Extrapolation >>
   end;
```

#### Computing differences

Next step is to compute differences and appending them to the corresponding
history data set.

```ada
      -- << Compute_Differences >>
      Index := History.First_Index;
      loop
         History.Append (New_Item => [], Count => 1);
         History (Index + 1).Reserve_Capacity (History (Index).Length + 1);
         for I in
           History (Index).First_Index .. History (Index).Last_Index - 1
         loop
            History (Index + 1).Append
              (History (Index) (I + 1) - History (Index) (I));
         end loop;
         exit when (for all Data of History (Index) => Data = 0);
         Index := Index + 1;
      end loop;
```

#### Computing extrapolate value

Finally, compute the extrapolate value, which must be added to the final
`Answer`:

```ada
      -- << Compute_Extrapolation >>
      for I in reverse History.First_Index .. History.Last_Index - 1 loop
         History (I).Append
           (History (I + 1).Last_Element + History (I).Last_Element);
      end loop;
      Answer := @ + History (History.First_Index).Last_Element;
```

### Part 2
[![Static Badge](https://img.shields.io/badge/read-part__2.adb-blue)](src/part_2.adb)

#### Extrapolate backwards

This part is pretty the same as part 1. The difference is how to compute the
extrapolate value:

```ada
      -- << Compute_Extrapolation >>
      for I in reverse History.First_Index .. History.Last_Index - 1 loop
         History (I).Prepend
           (History (I).First_Element - History (I + 1).First_Element);
      end loop;
      Answer := @ + History (History.First_Index).First_Element;
```

##
### License
MIT (c) 2023 Francesc Rocher
