with Aof.Core.Root_Objects;
with Aof.Core.Generic_Signals;

package Aof.Core.Signals is
   
   pragma Preelaborate;
   
   package Empty is new Aof.Core.Generic_Signals.S0
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object);
   
   package Access_Objects is new Aof.Core.Generic_Signals.S1
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      Param_1       => Aof.Core.Root_Objects.Access_Object);
   
   package Naturals is new Aof.Core.Generic_Signals.S1
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      Param_1       => Natural);
   
   package Integers is new Aof.Core.Generic_Signals.S1
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      Param_1       => Integer);
   
   package Floats is new Aof.Core.Generic_Signals.S1
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      Param_1       => Float);
   
end Aof.Core.Signals;
