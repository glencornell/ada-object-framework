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
with Aof.Core.Generic_Signals;
with Ada.Strings.Unbounded;

package Aof.Core.Signals is
   
   pragma Preelaborate;
   
   package Empty is new Aof.Core.Generic_Signals.S0
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object);
   
   package Access_Objects is new Aof.Core.Generic_Signals.S1
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      Param_1       => Aof.Core.Root_Objects.Access_Object);
   
   package Strings is new Aof.Core.Generic_Signals.S1
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      Param_1       => Ada.Strings.Unbounded.Unbounded_String);
   
   package Naturals is new Aof.Core.Generic_Signals.S1
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      Param_1       => Natural);
   
   package Integers is new Aof.Core.Generic_Signals.S1
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      Param_1       => Integer);
   
   package Floats is new Aof.Core.Generic_Signals.S1
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      Param_1       => Float);
   
   package Long_Floats is new Aof.Core.Generic_Signals.S1
     (Object        => Aof.Core.Root_Objects.Root_Object,
      Access_Object => Aof.Core.Root_Objects.Access_Object,
      Param_1       => Long_Float);
   
end Aof.Core.Signals;
