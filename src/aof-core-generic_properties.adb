package body Aof.Core.Generic_Properties is
   
   function Get (This : in Property'Class) return T is
   begin
      return This.Value;
   end;
   
   procedure Set (This : in out Property'Class;
		  Value : in T) is
   begin
      This.Value := Value;
      This.On_Change_Signal.Emit(This.Value);
   end;
   
   procedure Connect (This : in out Property'Class; 
		      Proc : in Signals_Pkg.Procedure_Ptr) is 
   begin
      This.On_Change_Signal.Connect(Proc);
   end;
   
   procedure Notify (This : in out Property'Class) is 
   begin
      This.On_Change_Signal.Emit(This.Value);
   end;
   
end Aof.Core.Generic_Properties;
