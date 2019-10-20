with Ada.Text_Io;
with Ada.Strings.Unbounded;

package body Labels is
   
   procedure Paint_Event (This : in out Label'Class) is
   begin
      Ada.Text_Io.Put_Line("Labels.Paint_Event() called");
      Ada.Text_Io.Put_Line("  Label_Text = """ & Ada.Strings.Unbounded.To_String(This.Label_Text.Get) & """");
   end;
   
end Labels;
