package Aof.Core.Abstract_Properties is
   
   pragma Pure;
   
   type Access_Mode is (RW, RO, WO);
   
   type Abstract_Property is abstract tagged limited record
      Access_Permissions : Access_Mode := RW;
   end record;
	 
   function Is_Readable (This : Abstract_Property) return Boolean is
      (case This.Access_Permissions is
	 when RW | RO => True,
	 when WO      => False);
      
   function Is_Writable (This : Abstract_Property) return Boolean is
      (case This.Access_Permissions is
	 when RW | WO => True,
	 when RO      => False);
      
end Aof.Core.Abstract_Properties;
