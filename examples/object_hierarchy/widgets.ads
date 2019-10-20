with Aof.Core.Objects;
with Aof.Core.Properties;

package Widgets is
   
   type Widget is new Aof.Core.Objects.Object with 
      record
	 --  Properties:
	 X      : Aof.Core.Properties.Naturals.Property;
	 Y      : Aof.Core.Properties.Naturals.Property;
	 Width  : Aof.Core.Properties.Naturals.Property;
	 Height : Aof.Core.Properties.Naturals.Property;
      end record;
   type Widget_Ptr is access all Widget'Class;
   
   --  Slots:
   procedure Paint_Event (This : in out Widget'Class);
   
end Widgets;
