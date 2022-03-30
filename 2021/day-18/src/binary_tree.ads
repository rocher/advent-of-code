generic

   type Element_Type is private;
   with function Parse_Element (Text : String) return Element_Type;

package Binary_Tree is

   type Node_Kind is (Node, Leaf);
   type Node_Type (K : Node_Kind) is private;
   type Node_Access is access all Node_Type;

   type Travers_Kind is (Pre_Order, In_Order, Post_Order);
   type Cursor is private;

   function Parse_Node
     (Text      : String; Node_Start : Character := '[';
      Separador : Character := ','; Node_End : Character := ']')
      return Node_Access;

   function Is_Root (N : Node_Access) return Boolean;
   function Is_Leaf (N : Node_Access) return Boolean;

   function Element (N : Node_Access) return Element_Type;
   function Level (N : Node_Access) return Natural;
   function Left (N : Node_Access) return Node_Access;
   function Right (N : Node_Access) return Node_Access;
   function Parent (N : Node_Access) return Node_Access;
   function Left_Most (N : Node_Access) return Node_Access;
   function Right_Most (N : Node_Access) return Node_Access;

   function Get_Cursor (N : Node_Access; K : Travers_Kind) return Cursor;
   function Has_Next (C : Cursor) return Boolean;
   function Next (C : Cursor) return Node_Access;

private
   type Node_Type (K : Node_Kind) is record
      Kind : Node_Kind;
      Parent : Node_Access;
      case K is
         when Leaf =>
            Element : Element_Type;
         when Node =>
            Left  : Node_Access;
            Right : Node_Access;
      end case;
   end record;

   type Cursor is record
      Travers : Travers_Kind;
      Current : Node_Access;
      Next    : Node_Access;
   end record;

end Binary_Tree;
