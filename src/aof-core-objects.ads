with Aof.Core.Signals;
with Aof.Core.Properties;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Strings.Unbounded;

package Aof.Core.Objects is
   
   --  Publicly accessible parts of the Object type. No objects should
   --  be made of this type.  Rather, see Object below.
   type Public_Part;
   type Public_Part_Ptr is access all Public_Part'Class;
   package Signals_With_Public_Part_Ptr is new Aof.Core.Signals.S1 (Param_1 => Public_Part_Ptr);
   type Public_Part is abstract new Ada.Finalization.Limited_Controlled with record
      Name : Aof.Core.Properties.Unbounded_Strings.Property;
      Destroyed : Signals_With_Public_Part_Ptr.Signal;
   end record;
   procedure Finalize (This : in out Public_Part);

   --  The object type is the root object in the Ada Object Framework.
   --  All objects should be derived from this type.
   type Object is limited new Public_Part with private;
   type Object_Ptr is access all Object'Class;
   package Object_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Object_Ptr);
   
   --  This exception is raised when an attempt is made to insert a
   --  parent as a child of an object, creating a circular hierarchy
   --  of objects.
   Circular_Reference_Exception : exception;
   
   --  Accessor to the object's name as a simple string.  This is a
   --  convenience subprogram that is the same as
   --  Ada.Strings.Unbounded.To_String(This.Object_Name.Get).
   function Get_Name (This : in Object'Class) return String;
   procedure Set_Name (This : in out Object'Class; Name : in String);
   
   --  Accessors to the object's parent.
   function Get_Parent (This : in Object'Class) return Object_Ptr;
   procedure Set_Parent -- raises Circular_Reference_Exception
     (This : in out Object'Class;
      Parent : in not null Object_Ptr);
   
   --  Accessors to the object's children.
   function Get_Children (This : in Object'Class) return Object_List.List;
   
   type Find_Child_Options is 
     (Find_Direct_Children_Only,
      Find_Children_Recursively);
   
   function Find_Child 
     (This : in Object'Class;
      Name : in Ada.Strings.Unbounded.Unbounded_String;
      Options : in Find_Child_Options := Find_Children_Recursively) return Object_Ptr;
   function Find_Child 
     (This : in Object'Class;
      Name : in String;
      Options : in Find_Child_Options := Find_Children_Recursively) return Object_Ptr;
   function Find_Children 
     (This : in Object'Class;
      Name : in Ada.Strings.Unbounded.Unbounded_String;
      Options : in Find_Child_Options := Find_Children_Recursively) return Object_List.List;
   function Find_Children 
     (This : in Object'Class;
      Name : in String;
      Options : in Find_Child_Options := Find_Children_Recursively) return Object_List.List;
   
   generic
      with procedure Proc(This : in out Object_Ptr);
   procedure Iterate
     (This : in Object_Ptr;
      Options : in Find_Child_Options := Find_Children_Recursively);
   
   function Contains
     (This : in out Object'Class;
      Child : in not null Object_Ptr) return Boolean;

private
   
   procedure Delete_Child
     (This : in out Object'Class;
      Child : in out not null Object_Ptr;
      Options : in Find_Child_Options := Find_Children_Recursively);
   
   --  This type is derived from Ada.Finalization.Limited_Controlled
   --  to ensure that memory is reclaimed when the object is destroyed
   --  or goes out of scope.  When the object is destroyed, so are all
   --  of its childern.
   type Object is limited new Public_Part with record
     Parent : Object_Ptr := null;
     Children : Object_List.List;
   end record;
   overriding procedure Finalize(This : in out Object);
   
end Aof.Core.Objects;
