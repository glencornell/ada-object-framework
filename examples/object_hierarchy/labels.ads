with Widgets;
with Aof.Core.Properties;

package Labels is
   
   type Label is new Widgets.Widget with 
      record
	 --  Properties:
	 Label_Text : Aof.Core.Properties.Unbounded_Strings.Property;
      end record;
   type Label_Ptr is access all Label'Class;
   
   --  Slots:
   procedure Paint_Event (This : in out Label'Class);
   
end Labels;
