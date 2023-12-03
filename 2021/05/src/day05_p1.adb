with Ada.Strings;         use Ada.Strings;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Day05_P1 is
  --  __Geometry_Definitions__
  subtype Coordinate_Range is Natural range 0 .. 999;

  type Point_Type is record
     X : Coordinate_Range;
     Y : Coordinate_Range;
  end record;

  type Segment_Type is record
     Start_Point : Point_Type;
     End_Point   : Point_Type;
  end record;

  type Ocean_Floor_Type is
    array (Coordinate_Range, Coordinate_Range) of Natural;
  --  __Ocean_Floor_And_Overlap_Sum__
  Ocean_Floor : Ocean_Floor_Type := (others => (others => 0));
  Σ_Overlap   : Natural          := 0;
  --  __Straight_Lines_Definition__
  function Is_Straight (S : Segment_Type) return Boolean is
    ((S.Start_Point.X = S.End_Point.X) or else (S.Start_Point.Y = S.End_Point.Y));
  --  __Drawing_Segments_Definition__
  procedure Draw_Segment (Vent : Segment_Type) is
     Point : Point_Type := Vent.Start_Point;

     -- compute deltas
     Δx : Integer :=
       (if Vent.Start_Point.X = Vent.End_Point.X then 0
        elsif Vent.Start_Point.X < Vent.End_Point.X then 1 else -1);

     Δy : Integer :=
       (if Vent.Start_Point.Y = Vent.End_Point.Y then 0
        elsif Vent.Start_Point.Y < Vent.End_Point.Y then 1 else -1);
  begin
     loop
        Ocean_Floor (Point.X, Point.Y) := @ + 1;

        if Ocean_Floor (Point.X, Point.Y) = 2 then
           Σ_Overlap := @ + 1;
        end if;

        exit when Point = Vent.End_Point;

        Point := (X => @.X + Δx, Y => @.Y + Δy);
     end loop;
  end Draw_Segment;
begin
  declare
    --  __Variables_To_Read_The_Input_File__
    Input : File_Type;
    Vent  : Segment_Type;
    Char  : Character;
    Sep   : String (1 .. 4); -- separator ' -> '
  begin
    Open (Input, In_File, "/home/ada/advent-of-code/public/2021/05/" & "input.txt");
    loop
      --  __Read_Vent_Segment_From_Input_File__
      Get (Input, Vent.Start_Point.X); Get (Input, Char); -- skip ','
      Get (Input, Vent.Start_Point.Y);

      Get (Input, Sep); -- skip ' -> '

      Get (Input, Vent.End_Point.X); Get (Input, Char); -- skip ','
      Get (Input, Vent.End_Point.Y);
      --  __Draw_Some_Vents__
      if Is_Straight (Vent) then
         Draw_Segment (Vent);
      end if;

      exit when End_Of_File (Input);
    end loop;
    Close (Input);
    --  __Export_Data_For_Plotting__
    declare
       Output : File_Type;
    begin
       Create (Output, Name => "/home/ada/advent-of-code/public/2021/05/" & "output_p1.txt");
       for Row in Ocean_Floor'Range (1) loop
          for Col in Ocean_Floor'Range (2) loop
             if Ocean_Floor (Row, Col) > 0 then
                Put_Line (Output, Row'Image & Col'Image &
                            Ocean_Floor (Row, Col)'Image);
             end if;
          end loop;
       end loop;
       Close (Output);
    end;
  end;

  Put_Line ("Answer:" & Σ_Overlap'Image);
end Day05_P1;
