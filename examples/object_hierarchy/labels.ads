with Ada.Strings.Unbounded;
with Widgets;
with Properties;

package Labels is
   
   package String_Properties is new Properties
     (T => Ada.Strings.Unbounded.Unbounded_String);

   type Label is new Widgets.Widget with 
      record
	 --  Properties:
	 Label_Text : String_Properties.Property;
      end record;
   type Label_Ptr is access all Label'Class;
   
   --  Slots:
   procedure Paint_Event (This : in out Label'Class);
   
end Labels;
