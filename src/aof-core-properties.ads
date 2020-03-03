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

with Aof.Core.Root_Objects;
with Aof.Core.Generic_Properties;
with Ada.Strings.Unbounded;

package Aof.Core.Properties is
   
   pragma Preelaborate;
   
   --  Instantiations of the Generic_Properties package of commonly
   --  used types.
   package Unbounded_Strings is new Aof.Core.Generic_Properties
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      T             => Ada.Strings.Unbounded.Unbounded_String);
   package Booleans is new Aof.Core.Generic_Properties 
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      T             => Boolean);
   package Naturals is new Aof.Core.Generic_Properties 
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      T             => Natural);
   package Integers is new Aof.Core.Generic_Properties 
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      T             => Integer);
   package Floats is new Aof.Core.Generic_Properties 
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      T             => Float);
   package Long_Floats is new Aof.Core.Generic_Properties 
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      T             => Long_Float);
   
end Aof.Core.Properties;
