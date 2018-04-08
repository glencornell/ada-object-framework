with Text_Io;

package body Slots is
   
   procedure Xaa is 
   begin
      Text_Io.Put_Line("Xaa() called");
   end;
   
   procedure Xab is 
   begin
      Text_Io.Put_Line("Xab() called");
   end;
   
   procedure Xac is 
   begin
      Text_Io.Put_Line("Xac() called");
   end;
   
   procedure S1a (Value : in Integer) is
   begin
      Text_Io.Put_Line("S1a(" & Value'Image & ") called");
   end;

   procedure S1b (Value : in Integer) is
   begin
      Text_Io.Put_Line("S1b(" & Value'Image & ") called");
   end;

   procedure S1c (Value : in Integer) is
   begin
      Text_Io.Put_Line("S1c(" & Value'Image & ") called");
   end;

end Slots;
