--with Ada.Dynamic_Priorities;
--with Ada.Task_Identification;

package body Aof.Core.Threads is
   
   task body Thread_Type is
   begin
      loop
	 accept Start (Priority : in Priority_Type) do
	    null; -- TODO: assign priority, name
	 end Start;
	 loop
	    select 
	       accept Quit;
	       -- TODO: tell the event loop to terminate, if running
	    or
	       accept Terminate_Task;
	       exit;
	    or
	       delay 0.1;
	    end select;
	 end loop;
      end loop;
   end Thread_Type;

end Aof.Core.Threads;
