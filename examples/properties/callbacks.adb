with Text_Io;

package body Callbacks is
   
   procedure On_Change (Value : in Integer) is
   begin
      Text_Io.Put_Line("On_Change(" & Value'Image & ") called");
   end;

end Callbacks;
