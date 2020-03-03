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

package body Aof.Core.Generic_Signals is
   
   package body S0 is
      
      procedure Connect (This : in out Signal'Class;
			 Object : in not null Access_Object;
			 Method : in not null Access_Method) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Method,
	       Object     => Object,
	       Method     => Method));
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Proc : in not null Access_Procedure) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Procedure,
	       Proc       => Proc));
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in not null Access_Signal) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Signal,
	       Signal     => Signal));
      end;
      
      procedure Emit (This : in Signal'Class) is
	 procedure Emit_Helper(Cursor : in Slot_Container_Pkg.Cursor) is
	    Item : constant Connections.Connection_Object_Type := Slot_Container_Pkg.Element(Cursor);
	 begin
	    case Item.Connection is
	       when Connections.Connect_To_Nothing =>
		  null;
	       when Connections.Connect_To_Method =>
		  Item.Method (Item.Object);
	       when Connections.Connect_To_Procedure =>
		  Item.Proc.all;
	       when Connections.Connect_To_Signal =>
		  Item.Signal.Emit;
	    end case;
	 end Emit_Helper;
      begin
	 This.Slots.Iterate(Emit_Helper'Access);
      end;
      
   end S0;
   
   package body S1 is
   
      procedure Connect (This : in out Signal'Class;
			 Object : in not null Access_Object;
			 Method : in not null Access_Method) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Method,
	       Object     => Object,
	       Method     => Method));
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Proc : in not null Access_Procedure) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Procedure,
	       Proc       => Proc));
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in not null Access_Signal) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Signal,
	       Signal     => Signal));
      end;
      
      procedure Emit 
	(This : in Signal'Class;
	 P1   : in Param_1) is
	 procedure Emit_Helper(Cursor : in Slot_Container_Pkg.Cursor) is
	    Item : constant Connections.Connection_Object_Type := Slot_Container_Pkg.Element(Cursor);
	 begin
	    case Item.Connection is
	       when Connections.Connect_To_Nothing =>
		  null;
	       when Connections.Connect_To_Method =>
		  Item.Method (Item.Object, P1);
	       when Connections.Connect_To_Procedure =>
		  Item.Proc (P1);
	       when Connections.Connect_To_Signal =>
		  Item.Signal.Emit (P1);
	    end case;
	 end Emit_Helper;
      begin
	 This.Slots.Iterate(Emit_Helper'Access);
      end;
      
   end S1;
   
   package body S2 is
         
      procedure Connect (This : in out Signal'Class;
			 Object : in not null Access_Object;
			 Method : in not null Access_Method) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Method,
	       Object     => Object,
	       Method     => Method));
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Proc : in not null Access_Procedure) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Procedure,
	       Proc       => Proc));
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in not null Access_Signal) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Signal,
	       Signal     => Signal));
      end;
      
      procedure Emit 
	(This : in Signal'Class;
	 P1   : in Param_1;
	 P2   : in Param_2) is
	 procedure Emit_Helper(Cursor : in Slot_Container_Pkg.Cursor) is
	    Item : constant Connections.Connection_Object_Type := Slot_Container_Pkg.Element(Cursor);
	 begin
	    case Item.Connection is
	       when Connections.Connect_To_Nothing =>
		  null;
	       when Connections.Connect_To_Method =>
		  Item.Method (Item.Object, P1, P2);
	       when Connections.Connect_To_Procedure =>
		  Item.Proc (P1, P2);
	       when Connections.Connect_To_Signal =>
		  Item.Signal.Emit (P1, P2);
	    end case;
	 end Emit_Helper;
      begin
	 This.Slots.Iterate(Emit_Helper'Access);
      end;
      
   end S2;
   
   package body S3 is
   
      procedure Connect (This : in out Signal'Class;
			 Object : in not null Access_Object;
			 Method : in not null Access_Method) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Method,
	       Object     => Object,
	       Method     => Method));
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Proc : in not null Access_Procedure) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Procedure,
	       Proc       => Proc));
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in not null Access_Signal) is
      begin
	 This.Slots.Append
	   (Connections.Connection_Object_Type'
	      (Connection => Connections.Connect_To_Signal,
	       Signal     => Signal));
      end;
      
      procedure Emit 
	(This : in Signal'Class;
	 P1   : in Param_1;
	 P2   : in Param_2;
	 P3   : in Param_3) is
	 procedure Emit_Helper(Cursor : in Slot_Container_Pkg.Cursor) is
	    Item : constant Connections.Connection_Object_Type := Slot_Container_Pkg.Element(Cursor);
	 begin
	    case Item.Connection is
	       when Connections.Connect_To_Nothing =>
		  null;
	       when Connections.Connect_To_Method =>
		  Item.Method (Item.Object, P1, P2, P3);
	       when Connections.Connect_To_Procedure =>
		  Item.Proc (P1, P2, P3);
	       when Connections.Connect_To_Signal =>
		  Item.Signal.Emit (P1, P2, P3);
	    end case;
	 end Emit_Helper;
      begin
	 This.Slots.Iterate(Emit_Helper'Access);
      end;
      
   end S3;
   
end Aof.Core.Generic_Signals;
