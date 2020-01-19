with Ada.Dynamic_Priorities;
with Ada.Task_Identification;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Conversion;

package body Aof.Core.Threads is
   
   use type System.Address;
   
   --  All this because ada.task_identification.task_id doesn't define "<"
   function To_Address is
      new Ada.Unchecked_Conversion
     (Ada.Task_Identification.Task_Id, System.Address);

   
   package Thread_Containers is new Ada.Containers.Ordered_Maps
     (Element_Type => Access_Thread,
      Key_Type     => System.Address);
   
   -- Singleton to manage all threads
   All_Threads : Thread_Containers.Map;
   
   procedure Initialize
     (This     : in out Thread) is
   begin
      This.Thread.Initialize(This'Unchecked_access);
      All_Threads.Insert(To_Address(This.Thread'Identity), This'Unchecked_access);
   end Initialize;
   
   procedure Start
     (This     : in out Thread;
      Priority : in System.Any_Priority) is
   begin
      This.Priority := Priority;
      This.Thread.Start(Priority);
   end Start;
   
   procedure Terminate_Thread
     (This     : in out Thread) is
   begin
      All_Threads.Delete (To_Address(This.Thread'Identity));
      Ada.Task_Identification.Abort_Task (This.Thread'Identity);
   end Terminate_Thread;
   
   procedure Run
     (This     : in out Thread) is
   begin
      -- TODO: call exec
      null;
   end Run;
   
   function Current_Thread return Access_Thread is
   begin
      return Thread_Containers.Element (All_Threads.Find 
	(To_Address (Ada.Task_Identification.Current_Task)));
   end Current_Thread;
   
   function Is_Finished
     (This     : in Thread) return Boolean is
   begin
      return This.Finished.Get;
   end Is_Finished;
   
   function Is_Running
     (This     : in Thread) return Boolean is
   begin
      return This.Started.Get and not This.Finished.Get;
   end Is_Running;
   
   function Priority
     (This     : in Thread) return System.Any_Priority is
   begin
      return This.Priority;
   end Priority;
   
   procedure Set_Priority
     (This     : in out Thread;
      Priority : in System.Any_Priority) is
   begin
      Ada.Dynamic_Priorities.Set_Priority 
	(Priority, This.Thread'Identity);
      This.Priority := Priority;
   end Set_Priority;
   
   task body Task_Type is
      This     : Access_Thread;
      Priority : System.Any_Priority;
   begin
      accept Initialize
	(The_Thread   : in Access_Thread) do
	 This     := The_Thread;
      end Initialize;
      This.Started.Set(False);
      This.Finished.Set(False);
      loop
	 accept Start 
	   (The_Priority : in System.Any_Priority) do
	    Priority := The_Priority;
	 end Start;
	 Ada.Dynamic_Priorities.Set_Priority 
	   (Priority, Ada.Task_Identification.Current_Task);
	 --  Notify any listeners that the task has started:
	 This.Started.Set(True);
	 
	 --  Run the event loop:
	 This.Run;
	 
	 --  Notify any listeners that the task has finished:
	 This.Finished.Set(False);
      end loop;
   end Task_Type;

end Aof.Core.Threads;
