with Aof.Core.Root_Objects;
with Aof.Core.Objects;

package Derived_Objects is
   
   type Derived_Object is new Aof.Core.Objects.Object with record
      Id : Natural;
   end record;
   type Access_Derived_Object is access all Derived_Object;
   
   procedure My_Slot (This : in Derived_Object);
   procedure My_Slot (This : in not null Aof.Core.Root_Objects.Access_Object);
   
end Derived_Objects;

