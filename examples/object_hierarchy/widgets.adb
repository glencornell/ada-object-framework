with Text_Io;

package body Widgets is
   
   procedure Paint_Event (This : in out Widget'Class) is
   begin
      Text_Io.Put_Line ("Widgets.Paint_Event() called");
      Text_Io.Put_Line ("  Object_Name = """ & This.Object_Name & """");
   end;
   
end Widgets;
