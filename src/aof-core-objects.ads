with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
with Aof.Core.Root_Objects;
with Aof.Core.Signals;
with Aof.Core.Properties;

package Aof.Core.Objects is
   
   pragma Preelaborate;
   
   --  Publicly accessible parts of the Object type. No objects should
   --  be made of this type.  Rather, see Object below.
   type Public_Part is abstract new Aof.Core.Root_Objects.Root_Object with record
      Name      : Aof.Core.Properties.Unbounded_Strings.Property;
      Destroyed : Aof.Core.Signals.Access_Objects.Signal;
   end record;
   procedure Finalize (This : in out Public_Part);

   --  The object type is the root object in the Ada Object Framework.
   --  All objects should be derived from this type.
   type Object is limited new Public_Part with private;
   type Access_Object is access all Object'Class;
   
   package Object_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Access_Object);
   
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
   function Get_Parent (This : in Object'Class) return Access_Object;
   procedure Set_Parent -- raises Circular_Reference_Exception
     (This   : in out Object'Class;
      Parent : in not null Access_Object);
   
   --  Accessors to the object's children.
   function Get_Children (This : in Object'Class) return Object_List.List;
   
   type Find_Child_Options is 
     (Find_Direct_Children_Only,
      Find_Children_Recursively);
   
   function Find_Child 
     (This    : in Object'Class;
      Name    : in Ada.Strings.Unbounded.Unbounded_String;
      Options : in Find_Child_Options := Find_Children_Recursively) return Access_Object;
   function Find_Child 
     (This    : in Object'Class;
      Name    : in String;
      Options : in Find_Child_Options := Find_Children_Recursively) return Access_Object;
   function Find_Children 
     (This    : in Object'Class;
      Name    : in Ada.Strings.Unbounded.Unbounded_String;
      Options : in Find_Child_Options := Find_Children_Recursively) return Object_List.List;
   function Find_Children 
     (This    : in Object'Class;
      Name    : in String;
      Options : in Find_Child_Options := Find_Children_Recursively) return Object_List.List;
   
   generic
      with procedure Proc(This : in out Access_Object);
   procedure Iterate
     (This    : in Access_Object;
      Options : in Find_Child_Options := Find_Children_Recursively);
   
   function Contains
     (This  : in out Object'Class;
      Child : in not null Access_Object) return Boolean;

private
   
   procedure Delete_Child
     (This    : in out Object'Class;
      Child   : in out not null Access_Object;
      Options : in Find_Child_Options := Find_Children_Recursively);
   
   --  This type is derived from Ada.Finalization.Limited_Controlled
   --  to ensure that memory is reclaimed when the object is destroyed
   --  or goes out of scope.  When the object is destroyed, so are all
   --  of its childern.
   type Object is limited new Public_Part with record
     Parent   : Access_Object := null;
     Children : Object_List.List;
   end record;
   overriding procedure Finalize(This : in out Object);
   
end Aof.Core.Objects;
