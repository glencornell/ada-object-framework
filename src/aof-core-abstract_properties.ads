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

package Aof.Core.Abstract_Properties is
   
   pragma Pure;
   
   type Access_Mode is (RW, RO, WO);
   
   type Abstract_Property is abstract tagged limited record
      Access_Permissions : Access_Mode := RW;
   end record;
	 
   function Is_Readable (This : Abstract_Property) return Boolean is
      (case This.Access_Permissions is
	 when RW | RO => True,
	 when WO      => False);
      
   function Is_Writable (This : Abstract_Property) return Boolean is
      (case This.Access_Permissions is
	 when RW | WO => True,
	 when RO      => False);
      
end Aof.Core.Abstract_Properties;
