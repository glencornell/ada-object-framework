with Signals;

generic
   type T is private;
package Properties is
   
   package Signals_Pkg is new Signals.S1 (Param_1 => T);
   
   type Property is tagged limited private;
   
   function Get (This : in Property'Class) return T;
   procedure Set (This : in out Property'Class;
		  Value : in T);
   procedure Connect (This : in out Property'Class; 
		      Proc : in Signals_Pkg.Procedure_Ptr);
   procedure Notify (This : in out Property'Class);
   
private
   
   type Property is tagged limited record
      Value : T;
      On_Change_Signal : Signals_Pkg.Signal;
   end record;
   
end Properties;
