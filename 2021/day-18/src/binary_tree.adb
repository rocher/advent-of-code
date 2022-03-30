with Ada.Unchecked_Deallocation;

package body Binary_Tree is
   function Parse_Node
     (Text : String; Node_Start : Character := '[';
      Separador : Character := ','; Node_End : Character := ']')
      return Node_Access
   is
      Node : Node_Access;
   begin
      if Text'Length = 1 then
         Node := new Node_Type (Leaf);
         -- Node.Element := Parse_Element ("0" & Text'First);
      end if;
      return Node;
   end Parse_Node;

   --  function Parse_Root (Text : String) return Node_Access is
   --     Node : Node_Access;
   --     I    : Natural;
   --  begin
   --     I          := Comma_Index (Text);
   --     Pair.Left  := Parse_Element (Text (Text'First + 1 .. I - 1));
   --     Pair.Right := Parse_Element (Text (I + 1 .. Text'Last - 1));
   --     return Pair;
   --  end Parse_Root;

   function Is_Root (N : Node_Access) return Boolean is (N.Parent = null);
   function Is_Leaf (N : Node_Access) return Boolean is (N.Kind = Leaf);

   function Element (N : Node_Access) return Element_Type is (N.Element);
   function Level (N : Node_Access) return Natural is
     (1 + (if Is_Root (N) then 0 else Level (N.Parent)));
   function Left (N : Node_Access) return Node_Access is (N.Left);
   function Right (N : Node_Access) return Node_Access is (N.Right);
   function Parent (N : Node_Access) return Node_Access is (N.Parent);

   function Left_Most (N : Node_Access) return Node_Access is
   begin
      return null;
   end Left_Most;

   function Right_Most (N : Node_Access) return Node_Access is
   begin
      return null;
   end Right_Most;

   function Get_Cursor (N : Node_Access; K : Travers_Kind) return Cursor is
      C : Cursor := (Travers => K, Current => null, Next => null);
   begin
      if Is_Leaf (N) then
         C.Current := N;
      else
         case C.Travers is
            when Pre_Order =>
               C.Current := N;
               C.Next := Left (C.Current);
            when In_Order =>
               C.Current := Left_Most (N);
               C.Next := Parent (C.Current);
            when Post_Order =>
               C.Current := Left_Most (N);
               C.Next := Right_Most (Parent (C.Current));
         end case;
      end if;
      return C;
   end Get_Cursor;

   function Has_Next (C : Cursor) return Boolean is (C.Next /= null);
   function Next (C : Cursor) return Node_Access is
   begin
      return null;
   end Next;

end Binary_Tree;
