with Aof.Core.Root_Objects;
with Aof.Core.Generic_Properties;
with Ada.Strings.Unbounded;

package Aof.Core.Properties is
   
   pragma Preelaborate;
   
   --  Instantiations of the Generic_Properties package of commonly
   --  used types.
   package Unbounded_Strings is new Aof.Core.Generic_Properties
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      T             => Ada.Strings.Unbounded.Unbounded_String);
   package Booleans is new Aof.Core.Generic_Properties 
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      T             => Boolean);
   package Naturals is new Aof.Core.Generic_Properties 
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      T             => Natural);
   package Integers is new Aof.Core.Generic_Properties 
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      T             => Integer);
   package Floats is new Aof.Core.Generic_Properties 
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      T             => Float);
   
end Aof.Core.Properties;
