with Aof.Core.Generic_Properties;
with Ada.Strings.Unbounded;

package Aof.Core.Properties is
      
   --  Instantiations of the Generic_Properties package of commonly
   --  used types.
   package Unbounded_Strings is new Aof.Core.Generic_Properties
     (T => Ada.Strings.Unbounded.Unbounded_String);
   package Naturals is new Aof.Core.Generic_Properties 
     (T => Natural);
   
end Aof.Core.Properties;
