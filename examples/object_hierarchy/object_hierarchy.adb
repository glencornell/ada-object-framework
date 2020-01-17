with Ada.Strings.Unbounded.Text_Io;
with Ada.Text_Io;
with Aof.Core.Objects;
with My_Objects;

--  This example highlights what I envision to be a typical use of the
--  object system.  Here we define a widget object type (in the
--  package Widgets), which is inherited from the Objects.Object base
--  class.  We then create several widgets and place them in an object
--  hierarchy (different than the inheritance hierarchy).  We then
--  repaint the widgets.

procedure Object_Hierarchy is
   
   procedure Dump_Object (This : in out Aof.Core.Objects.Access_Object) is 
   begin
      Ada.Text_Io.Put_Line("Object: " & This.Get_Name);
   end;
   
   procedure Dump_Object_Tree is new Aof.Core.Objects.Iterate(Proc => Dump_Object);
   
begin
   
   --  Set the object_name properties of the widgets:
   My_Objects.Top.Set_Name("Top");
   My_Objects.Form.Set_Name("Form");
   My_Objects.Label.Set_Name("Label");
   My_Objects.Row_Column_Layout.Set_Name("Row_Column_Layout");
   My_Objects.Ok.Set_Name("Ok");
   My_Objects.Cancel.Set_Name("Cancel");
   
   --  Construct the object hierarchy:
   My_Objects.Form.Set_Parent              (My_Objects.Top_Ptr);
   My_Objects.Label.Set_Parent             (My_Objects.Form_Ptr);
   My_Objects.Row_Column_Layout.Set_Parent (My_Objects.Form_Ptr);
   My_Objects.Ok.Set_Parent                (My_Objects.Row_Column_Layout_Ptr);
   My_Objects.Cancel.Set_Parent            (My_Objects.Row_Column_Layout_Ptr);
   
   --  Set the label text:
   My_Objects.Label.Label_Text.Set(Ada.Strings.Unbounded.To_Unbounded_String("Hello, World!"));
   
   --  Print the label text for kicks:
   Ada.Strings.Unbounded.Text_Io.Put_Line (My_Objects.Label.Label_Text.Get);
   
   --  Dump the widget hierarchy:
   Dump_Object_Tree(My_Objects.Top_Ptr);
   
end Object_Hierarchy;
