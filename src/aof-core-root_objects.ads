with Ada.Finalization;

package Aof.Core.Root_Objects is
   
   pragma Preelaborate;
   
   type Root_Object is abstract new Ada.Finalization.Limited_Controlled with null record;
   type Access_Object is access all Root_Object'Class;
   
end Aof.Core.Root_Objects;
