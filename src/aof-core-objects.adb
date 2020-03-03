--  Copyright (C) 2020 Glen Cornell <glen.m.cornell@gmail.com>
--  
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation, either version 3 of the
--  License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.

package body Aof.Core.Objects is
   
   use type Object_List.Cursor;
   use type Ada.Strings.Unbounded.Unbounded_String;
   
   function Get_Name (This : in Object'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String(This.Name.Get);
   end;
   
   procedure Set_Name 
     (This : in out Object'Class;
      Name : in String) is
   begin
      This.Name.Set(Ada.Strings.Unbounded.To_Unbounded_String(Name));
   end;
   
   function Get_Parent (This : in Object'Class) return Access_Object is
   begin
      return This.Parent;
   end;
   
   procedure Set_Parent
     (This     : in out Object'Class;
      Parent   : in not null Access_Object) is
      This_Ptr : Access_Object := This'Unchecked_Access;
   begin
      -- If the parent is found in this objects list of children(recursively), then fail
      if This.Contains(Parent) or This_Ptr = Parent then
	 raise Circular_Reference_Exception;
      end if;
      
      -- unlink this from its existing parent
      if This.Parent /= null then
	 This.Parent.Delete_Child(This_Ptr);
      end if;
      
      -- add the object "This" to the "Children" container belonging
      -- to the object "Parent"
      Parent.Children.Append(New_Item => This_Ptr);
      This.Parent := Parent;
   end;
   
   function Get_Children 
     (This : in Object'Class) return Object_List.List is
   begin
      return This.Children;
   end;
   
   function Find_Child 
     (This    : in Object'Class;
      Name    : in Ada.Strings.Unbounded.Unbounded_String;
      Options : in Find_Child_Options := Find_Children_Recursively) return Access_Object is 
   begin
      for Obj of This.Children loop
	 if Name = Obj.Name.Get then
	    return Obj;
	 end if;
	 if Options = Find_Children_Recursively then
	    return Obj.Find_Child(Name, Options);
	 end if;
      end loop;
      return null;
   end;
   
   function Find_Child 
     (This    : in Object'Class;
      Name    : in String;
      Options : in Find_Child_Options := Find_Children_Recursively) 
     return Access_Object is 
      The_Name : constant Ada.Strings.Unbounded.Unbounded_String := 
	Ada.Strings.Unbounded.To_Unbounded_String(Name);
   begin
      return This.Find_Child(The_Name, Options);
   end;
   
   function Find_Children
     (This    : in Object'Class;
      Name    : in Ada.Strings.Unbounded.Unbounded_String;
      Options : in Find_Child_Options := Find_Children_Recursively) 
     return Object_List.List is 
      Obj_List : Object_List.List;
   begin
      for Obj of This.Children loop
	 if Name = Obj.Name.Get then
	    Obj_List.Append(Obj);
	 end if;
	 if Options = Find_Children_Recursively then
	    declare
	       Children : Object_List.List := Obj.Find_Children(Name, Options);
	    begin 
	       Obj_List.Splice(Before => Object_List.No_Element, 
			       Source => Children);
	    end;
	 end if;
      end loop;
      return Obj_List;
   end;
   
   function Find_Children
     (This    : in Object'Class;
      Name    : in String;
      Options : in Find_Child_Options := Find_Children_Recursively)
     return Object_List.List is 
      The_Name : constant Ada.Strings.Unbounded.Unbounded_String := 
	Ada.Strings.Unbounded.To_Unbounded_String(Name);
   begin
      return This.Find_Children(The_Name, Options);
   end;
   
   procedure Iterate
     (This    : in Access_Object;
      Options : in Find_Child_Options := Find_Children_Recursively) is
   begin
      for Child of This.Children loop
	 if Options = Find_Children_Recursively then
	    Iterate(This => Child, Options => Options);
	 end if;
	 Proc(Child);
      end loop;
   end;
   
   function Contains
     (This  : in out Object'Class;
      Child : in not null Access_Object)
     return Boolean is 
      This_Ptr : constant Access_Object := This'Unchecked_Access;
      Obj : Access_Object := Child.Parent;
   begin
      while Obj /= null loop
	 if Obj = This_Ptr then
	    return True;
	 end if;
	 Obj := Obj.Parent;
      end loop;
      return False;
   end;
   
   procedure Delete_Child
     (This    : in out Object'Class;
      Child   : in out not null Access_Object;
      Options : in Find_Child_Options := Find_Children_Recursively) is 
      
      I : Object_List.Cursor := This.Children.First;
      Obj : Access_Object := null;
   begin
      loop
	 Obj := Object_List.Element(I);
	 if Obj = Child then
	    Obj.Parent := null;
	    This.Children.Delete(I);
	    return;
	 end if;
	 if Options = Find_Children_Recursively then
	    Obj.Delete_Child(Child, Options);
	 end if;
	 exit when I = This.Children.Last;
	 I := Object_List.Next(I);
      end loop;
   end;
   
   procedure Finalize (This : in out Public_Part) is
   begin
      This.Destroyed.Emit(This'Unchecked_Access);
   end Finalize;
   
   procedure Finalize (This : in out Object) is
   begin
      -- TODO: delete all children?
      null;
   end Finalize;
   
end Aof.Core.Objects;
