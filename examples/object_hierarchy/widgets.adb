with Ada.Text_Io;

package body Widgets is
   
   procedure Paint_Event (This : in out Widget'Class) is
   begin
      Ada.Text_Io.Put_Line ("Widgets.Paint_Event() called");
      Ada.Text_Io.Put_Line ("  Object_Name = """ & This.Get_Name & """");
   end;
   
end Widgets;
