with Ada.Text_Io;

package body Callbacks is
   
   procedure On_Change (Value : in Integer) is
   begin
      Ada.Text_Io.Put_Line("On_Change(" & Value'Image & ") called");
   end;

end Callbacks;
