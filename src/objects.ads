with Properties;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package Objects is
   
   --  This exception is raised when an attempt is made to insert a
   --  parent as a child of an object, creating a circular hierarchy
   --  of objects.
   Circular_Reference_Exception : exception;
   
   type Find_Child_Options is 
     (Find_Direct_Children_Only,
      Find_Children_Recursively);
   
   type Object is tagged limited private;
   type Object_Ptr is access all Object'Class;
   package Object_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Object_Ptr);
   
   --  Accessors to the object name.
   function Object_Name (This : in Object'Class) return String;
   procedure Set_Object_Name 
     (This : in out Object'Class;
      Name : in String);
   
   --  Accessors to the given object's parent.
   function Parent (This : in Object'Class) return Object_Ptr;
   procedure Set_Parent -- raises Circular_Reference_Exception
     (This : in out Object'Class;
      Parent : in out not null Object_Ptr);
   
   --  Accessors to the given object's children.
   function Children (This : in Object'Class) return Object_List.List;
   function Find_Child 
     (This : in Object'Class;
      Name : in String;
      Options : in Find_Child_Options := Find_Children_Recursively) return Object_Ptr;
   function Find_Children 
     (This : in Object'Class;
      Name : in String;
      Options : in Find_Child_Options := Find_Children_Recursively) return Object_List.List;
   generic
      with procedure Proc(This : in out Object_Ptr);
   procedure Iterate
     (This : in out Object_Ptr;
      Options : in Find_Child_Options := Find_Children_Recursively);
   
private
   
   function Contains
     (This : in out Object'Class;
      Child : in not null Object_Ptr) return Boolean;

   procedure Delete_Child
     (This : in out Object'Class;
      Child : in out not null Object_Ptr;
      Options : in Find_Child_Options := Find_Children_Recursively);
   
   package String_Property is new Properties(T => Ada.Strings.Unbounded.Unbounded_String);
   
   type Object is tagged limited record
      Object_Name : String_Property.Property;
      Parent : Object_Ptr := null;
      Children : Object_List.List;
   end record;
   
end Objects;
