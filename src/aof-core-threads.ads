with Aof.Core.Objects;
with Aof.Core.Signals;

package Aof.Core.Threads is
   
   pragma Preelaborate;
   
   type Priority_Type is
     (Idle_Priority,          -- scheduled only when no other threads are running.
      Lowest_Priority,        -- scheduled less often than LowPriority.
      Low_Priority,           -- scheduled less often than NormalPriority.
      Normal_Priority,        -- the default priority of the operating system.
      High_Priority,          -- scheduled more often than NormalPriority.
      Highest_Priority,       -- scheduled more often than HighPriority.
      Time_Critical_Priority, -- scheduled as often as possible.
      Inherit_Priority        -- use the same priority as the creating thread. This is the default.
     );
   
   --  This is the public part of the thread ibject.  do not create
   --  instances of this type.
   type Thread_Public_Part is limited new Aof.Core.Objects.Object with record
     Started  : Aof.Core.Signals.Empty.Signal;  --  Emitted when the task is started
     Finished : Aof.Core.Signals.Empty.Signal;  --  Emitted when the task is finished
   end record;
   
   type Thread is limited new Thread_Public_Part with private;
   
private
   
   task type Thread_Type is 
      entry Start (Priority : in Priority_Type);
      entry Quit;
      entry Terminate_Task;
   end Thread_Type;
   
   type Thread is limited new Thread_Public_Part with record
     Thread : Thread_Type;
   end record;
   
end Aof.Core.Threads;
