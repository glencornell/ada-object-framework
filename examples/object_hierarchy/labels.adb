with Text_Io;

package body Labels is
   
   procedure Paint_Event (This : in out Label'Class) is
   begin
      Text_Io.Put_Line("Labels.Paint_Event() called");
      Text_Io.Put_Line("  Label_Text = """ & Ada.Strings.Unbounded.To_String(This.Label_Text.Get) & """");
   end;
   
end Labels;
