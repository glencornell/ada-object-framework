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

with System;
with Aof.Core.Objects;
with Aof.Core.Properties;

package Aof.Core.Threads is
   
--   pragma Preelaborate;
   
   --  This is the public part of the thread object.  do not create
   --  instances of this type.
   type Thread_Public_Part is limited new Aof.Core.Objects.Object with record
     Started  : Aof.Core.Properties.Booleans.Property;
     Finished : Aof.Core.Properties.Booleans.Property;
   end record;
   
   type Thread is limited new Thread_Public_Part with private;
   type Access_Thread is access all Thread'Class;
   
   procedure Initialize
     (This     : in out Thread);
   procedure Start
     (This     : in out Thread;
      Priority : in System.Any_Priority);
   procedure Terminate_Thread
     (This     : in out Thread);
   procedure Run
     (This     : in out Thread);
   function Current_Thread return Access_Thread;
   function Is_Finished
     (This     : in Thread) return Boolean;
   function Is_Running
     (This     : in Thread) return Boolean;
   function Priority
     (This     : in Thread) return System.Any_Priority;
   procedure Set_Priority
     (This     : in out Thread;
      Priority : in System.Any_Priority);
private
   
   task type Task_Type Is 
      entry Initialize 
	(The_Thread   : in Access_Thread);
      entry Start 
	(The_Priority : in System.Any_Priority);
   end Task_Type;
   
   type Thread is limited new Thread_Public_Part with record
     Thread   : Task_Type;
     Priority : System.Any_Priority;
   end record;
   
end Aof.Core.Threads;
