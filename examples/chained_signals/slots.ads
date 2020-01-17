with Aof.Core.Signals;
with Derived_Objects;

package Slots is
   procedure Xaa;
   procedure Xab;
   procedure Xac;
   Chained_Signal : aliased Aof.Core.Signals.Empty.Signal;
   
   Obj_1 : aliased Derived_Objects.Derived_Object;
   Obj_2 : aliased Derived_Objects.Derived_Object;
   
end Slots;
