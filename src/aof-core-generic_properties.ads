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

with Aof.Core.Generic_Signals;
with Aof.Core.Abstract_Properties;

generic
   type Object is abstract tagged limited private;
   type Access_Object is access all Object'Class;
   type T is private;
package Aof.Core.Generic_Properties is
   
   pragma Preelaborate;
   
   package Signals_Pkg is new Aof.Core.Generic_Signals.S1 
     (Object        => Object,
      Access_Object => Access_Object,
      Param_1       => T);
   
   type Property is limited new Aof.Core.Abstract_Properties.Abstract_Property with private;
   
   function Get (This : in Property'Class) return T
     with Pre => This.Is_Readable;
   
   procedure Set (This : in out Property'Class;
		  Value : in T) 
     with Pre => This.Is_Writable;
   
   procedure Connect (This : in out Property'Class; 
		      Proc : in Signals_Pkg.Access_Procedure);
   
   procedure Notify (This : in out Property'Class);
   
private
   
   type Property is limited new Aof.Core.Abstract_Properties.Abstract_Property with record
      Value            : T;
      On_Change_Signal : Signals_Pkg.Signal;
   end record;
   
end Aof.Core.Generic_Properties;
