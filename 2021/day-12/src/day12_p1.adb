with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Day12_P1 is
   subtype Cave_Type is String (1 .. 2);
   type Cave_Access is not null access all Cave_Type;

   Start_Cave : constant Cave_Type := "aa";
   End_Cave   : constant Cave_Type := "zz";

   function Is_Big (Cave : Cave_Type) return Boolean is (Is_Upper (Cave (1)));
   function Is_Small (Cave : Cave_Type) return Boolean is (not Is_Big (Cave));

   function To_Cave (S : String) return Cave_Type is
   begin
      if S = "start" then
         return Start_Cave;
      elsif S = "end" then
         return End_Cave;
      else
         return S;
      end if;
   end To_Cave;

   function Cave_Hash (Cave : Cave_Type) return Hash_Type is
      Hash : Hash_Type := 0;
   begin
      for I in Cave_Type'Range loop
         Hash := Hash * Hash_Type (Character'Pos (Cave (I)));
      end loop;
      return Hash;
   end Cave_Hash;

   package Cave_List_Package is new Doubly_Linked_Lists
     (Element_Type => Cave_Type, "=" => "=");
   use Cave_List_Package;
   subtype Cave_List_Type is Cave_List_Package.List;

   package Cave_Map_Package is new Ada.Containers.Hashed_Maps
     (Key_Type => Cave_Type, Element_Type => Cave_List_Type, Hash => Cave_Hash,
      Equivalent_Keys => "=", "=" => "=");
   use Cave_Map_Package;

   subtype Cave_Map_Type is Cave_Map_Package.Map;
   subtype Cave_Ref_Type is Cave_Map_Package.Reference_Type;

   function Visitable
      (Visited_Caves : Cave_List_Type; Cave : Cave_Type) return Boolean
   is
   begin
      if Is_Big (Cave) then
         --  big caves can be visited any number of times
         return True;
      else
         --  small caves can be visited only once
         return Visited_Caves.Find (Cave) = Cave_List_Package.No_Element;
      end if;
   end Visitable;

   function Adjacent_Caves
     (Cave_Map : Cave_Map_Type; Visited_Caves : Cave_List_Type;
      Src      : Cave_Type) return Cave_List_Type
   is
      Dst_List : Cave_List_Type;
   begin
      for Dst of Cave_Map (Src) loop
         if Visitable (Visited_Caves, Dst) then
            Dst_List.Append (Dst);
         end if;
      end loop;

      return Dst_List;
   end Adjacent_Caves;

   function Explore_Paths
     (Cave_Map : Cave_Map_Type; Visited_Caves : in out Cave_List_Type;
      Src      : Cave_Type) return Natural
   is
      Dst_List : Cave_List_Type;
      Paths    : Natural := 0;
   begin
      Visited_Caves.Append (Src);

      Dst_List := Adjacent_Caves (Cave_Map, Visited_Caves, Src);
      for Dst of Dst_List loop
         if Dst = End_Cave then
            Paths := Paths + 1;
         else
            Paths := Paths + Explore_Paths (Cave_Map, Visited_Caves, Dst);
         end if;
      end loop;

      Visited_Caves.Delete_Last;
      return Paths;
   end Explore_Paths;

   Input    : File_Type;
   Text     : String (1 .. 16);
   Last_Pos : Natural;
   I        : Natural;

   Cave_Map      : Cave_Map_Type;
   Cave_Path     : Cave_List_Type;
   Src, Dst      : Cave_Type;
   Visited_Caves : Cave_List_Type;
   Paths         : Natural := 0;

begin
   Open (Input, In_File, "input");
   while not End_Of_File (Input) loop
      Text := (others => ' ');
      Get_Line (Input, Text, Last_Pos);
      I   := Index (Text, "-", 1);
      Src := To_Cave (Text (1 .. I - 1));
      Dst := To_Cave (Text (I + 1 .. Last_Pos));

      if Src /= End_Cave and Dst /= Start_Cave then
         if Cave_Map.Find (Src) = Cave_Map_Package.No_Element then
            Cave_Map.Insert (Src, Cave_List_Package.Empty_List);
         end if;
         Cave_Map (Src).Append (Dst);
      end if;

      if Src /= Start_Cave and Dst /= End_Cave then
         if Cave_Map.Find (Dst) = Cave_Map_Package.No_Element then
            Cave_Map.Insert (Dst, Cave_List_Package.Empty_List);
         end if;
         Cave_Map (Dst).Append (Src);
      end if;
   end loop;
   Close (Input);

   Visited_Caves := Cave_List_Package.Empty_List;
   Paths         := Explore_Paths (Cave_Map, Visited_Caves, Start_Cave);

   Put_Line ("Answer:" & Paths'Image);
end Day12_P1;
