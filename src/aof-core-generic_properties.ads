with Aof.Core.Generic_Signals;
with Aof.Core.Abstract_Properties;

generic
   type Object is abstract tagged limited private;
   type Access_Object is access all Object'Class;
   type T is private;
package Aof.Core.Generic_Properties is
   
   pragma Preelaborate;
   
   package Signals_Pkg is new Aof.Core.Generic_Signals.S1 
     (Object        => Object,
      Access_Object => Access_Object,
      Param_1       => T);
   
   type Property is limited new Aof.Core.Abstract_Properties.Abstract_Property with private;
   
   function Get (This : in Property'Class) return T
     with Pre => This.Is_Readable;
   
   procedure Set (This : in out Property'Class;
		  Value : in T) 
     with Pre => This.Is_Writable;
   
   procedure Connect (This : in out Property'Class; 
		      Proc : in Signals_Pkg.Access_Procedure);
   
   procedure Notify (This : in out Property'Class);
   
private
   
   type Property is limited new Aof.Core.Abstract_Properties.Abstract_Property with record
      Value            : T;
      On_Change_Signal : Signals_Pkg.Signal;
   end record;
   
end Aof.Core.Generic_Properties;
