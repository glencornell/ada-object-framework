with Ada.Text_Io;

package body Derived_Objects is
   
   procedure My_Slot (This : in Derived_Object) is
   begin
      Ada.Text_Io.Put_Line ("My_Slot called for object with id = " & Natural'Image(This.Id));
   end My_Slot;
   
   procedure My_Slot (This : in not null Aof.Core.Root_Objects.Access_Object) is
   begin
      Access_Derived_Object(This).My_Slot;
   end My_Slot;
   
end Derived_Objects;

