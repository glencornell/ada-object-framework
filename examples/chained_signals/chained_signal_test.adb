with Aof.Core.Signals;
with Slots;

--  This example builds upon the simple signal example where signals
--  may be chained ad-infintum.  In some circumstances, it may be
--  useful to connect one signal to another.  This is one way to
--  acheive the Delegation GoF design pattern.

procedure Chained_Signal_Test is

   package S1_Pkg is new Aof.Core.Signals.S1(Param_1 => Integer);

   S0 : Aof.Core.Signals.S0.Signal;
   S1 : S1_Pkg.Signal;
begin
   --  Connecting the slots to the signal as in the first example:
   S0.Connect(Slots.Xaa'access);
   S0.Connect(Slots.Xab'access);
   S0.Connect(Slots.Xac'access);
   
   --  Connecting the same set of slots to the other signal
   --  (Chained_Signal):
   Slots.Chained_Signal.Connect(Slots.Xac'access);
   Slots.Chained_Signal.Connect(Slots.Xab'access);
   Slots.Chained_Signal.Connect(Slots.Xaa'access);
   
   --  Connect the signal Chained_Signal to the first signal, as if
   --  Chained_Signal were a slot:
   S0.Connect(Signal => Slots.Chained_Signal'access);
   
   --  Now emit the signal:
   S0.Emit;
end;
