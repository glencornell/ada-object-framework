with Aof.Core.Root_Objects;
with Aof.Core.Generic_Signals;
with Aof.Core.Signals;
with Slots;

--  In this example, we will create two signals, one with no arguments
--  and another with one argument.  We use the "Connect" method to
--  connect a slot (A.K.A. callback) to a signal.  We then invoke the
--  signal's "Emit" method to subsequently invoke all slots connected
--  to the signal.

procedure Signal_Example is
   
   --  Create a signal class containing one argument of type integer
   package S1_Pkg is new Aof.Core.Generic_Signals.S1
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      Param_1       => Integer);
   
   S0 : Aof.Core.Signals.Empty.Signal;
   S1 : S1_Pkg.Signal;
   
begin
   --  Connect the slots
   S0.Connect(Slots.Xaa'access);
   S0.Connect(Slots.Xab'access);
   S0.Connect(Slots.Xac'access);
   --  Emit the signal
   S0.Emit;
   
   --  Connect the slots to the single argument signal.  Note that the
   --  argument count and type must match that of the signal or you
   --  will get a compiler error.
   S1.Connect(Slots.S1a'access);
   S1.Connect(Slots.S1b'access);
   S1.Connect(Slots.S1c'access);
   --  Emit the signal with different values
   for I in 1 .. 3 loop
      S1.Emit(I);
   end loop;
   
end Signal_Example;
