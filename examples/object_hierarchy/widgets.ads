with Objects;
with Properties;

package Widgets is
   
   package Natural_Properties is new Properties 
     (T => Natural);

   type Widget is new Objects.Object with 
      record
	 --  Properties:
	 X : Natural_Properties.Property;
	 Y : Natural_Properties.Property;
	 Width : Natural_Properties.Property;
	 Height : Natural_Properties.Property;
      end record;
   type Widget_Ptr is access all Widget'Class;
   
   --  Slots:
   procedure Paint_Event (This : in out Widget'Class);
   
end Widgets;
