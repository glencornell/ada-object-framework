with Aof.Core.Signals;
with Derived_Objects;
with Slots;

--  This example builds upon the simple signal example where signals
--  may be chained ad-infintum.  In some circumstances, it may be
--  useful to connect one signal to another.  This is one way to
--  acheive the Delegation GoF design pattern.

procedure Chained_Signals is
   S0 : Aof.Core.Signals.Empty.Signal;
begin
   --  Assign values to each of the objects to distinguish between
   --  them.
   Slots.Obj_1.Id := 1;
   Slots.Obj_2.Id := 2;
   
   --  Connecting the slots to the signal as in the first example:
   S0.Connect(Slots.Xaa'access);
   S0.Connect(Slots.Xab'access);
   S0.Connect(Slots.Xac'access);
   S0.Connect(Slots.Obj_1'Access, Derived_Objects.My_Slot'Access);
   S0.Connect(Slots.Obj_2'Access, Derived_Objects.My_Slot'Access);
   
   --  Connecting the same set of slots in a different order to the
   --  other signal
   Slots.Chained_Signal.Connect(Slots.Xac'access);
   Slots.Chained_Signal.Connect(Slots.Xab'access);
   Slots.Chained_Signal.Connect(Slots.Obj_2'Access, Derived_Objects.My_Slot'access);
   Slots.Chained_Signal.Connect(Slots.Xaa'access);
   Slots.Chained_Signal.Connect(Slots.Obj_1'Access, Derived_Objects.My_Slot'access);
   
   --  Connect the signal Chained_Signal to the first signal, as if
   --  Chained_Signal were a slot:
   S0.Connect(Signal => Slots.Chained_Signal'access);
   
   --  Now emit the signal:
   S0.Emit;
   
end Chained_Signals;
