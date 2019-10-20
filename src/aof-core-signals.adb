package body Aof.Core.Signals is
   
   package body S0 is
      
      procedure Connect (This : in out Signal'Class; 
			 Proc : in Procedure_Ptr) is
      begin
	 This.Function_Pool.Append(Proc);
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in Signal_Ptr) is
      begin
	 This.Signal_Pool.Append(Signal);
      end;
      
      procedure Emit (This : in Signal'Class) is
	 procedure Procedure_Emit_Helper(Cursor : in Procedure_Container_Pkg.Cursor) is
	 begin
	    Procedure_Container_Pkg.Element(Cursor).all;
	 end;
	 procedure Signal_Emit_Helper(Cursor : in Signal_Container_Pkg.Cursor) is
	 begin
	    Signal_Container_Pkg.Element(Cursor).all.Emit;
	 end;
      begin
	 This.Function_Pool.Iterate(Procedure_Emit_Helper'Access);
	 This.Signal_Pool.Iterate(Signal_Emit_Helper'Access);
      end;
      
   end S0;
   
   package body S1 is
   
      procedure Connect (This : in out Signal'Class; 
			 Proc : in Procedure_Ptr) is
      begin
	 This.Function_Pool.Append(Proc);
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in Signal_Ptr) is
      begin
	 This.Signal_Pool.Append(Signal);
      end;
      
      procedure Emit (This : in Signal'Class;
		      P1 : in Param_1) is
	 procedure Procedure_Emit_Helper(Cursor : in Procedure_Container_Pkg.Cursor) is
	 begin
	    Procedure_Container_Pkg.Element(Cursor).all(P1);
	 end;
	 procedure Signal_Emit_Helper(Cursor : in Signal_Container_Pkg.Cursor) is
	 begin
	    Signal_Container_Pkg.Element(Cursor).all.Emit(P1);
	 end;
      begin
	 This.Function_Pool.Iterate(Procedure_Emit_Helper'Access);
	 This.Signal_Pool.Iterate(Signal_Emit_Helper'Access);
      end;
      
   end S1;
   
   package body S2 is
   
      procedure Connect (This : in out Signal'Class; 
			 Proc : in Procedure_Ptr) is
      begin
	 This.Function_Pool.Append(Proc);
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in Signal_Ptr) is
      begin
	 This.Signal_Pool.Append(Signal);
      end;
      
      procedure Emit (This : in Signal'Class;
		      P1 : in Param_1;
		      P2 : in Param_2) is
	 procedure Procedure_Emit_Helper(Cursor : in Procedure_Container_Pkg.Cursor) is
	 begin
	    Procedure_Container_Pkg.Element(Cursor).all(P1, P2);
	 end;
	 procedure Signal_Emit_Helper(Cursor : in Signal_Container_Pkg.Cursor) is
	 begin
	    Signal_Container_Pkg.Element(Cursor).all.Emit(P1, P2);
	 end;
      begin
	 This.Function_Pool.Iterate(Procedure_Emit_Helper'Access);
	 This.Signal_Pool.Iterate(Signal_Emit_Helper'Access);
      end;
      
   end S2;
   
   package body S3 is
   
      procedure Connect (This : in out Signal'Class; 
			 Proc : in Procedure_Ptr) is
      begin
	 This.Function_Pool.Append(Proc);
      end;
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in Signal_Ptr) is
      begin
	 This.Signal_Pool.Append(Signal);
      end;
      
      procedure Emit (This : in Signal'Class;
		      P1 : in Param_1;
		      P2 : in Param_2;
		      P3 : in Param_3) is
	 procedure Procedure_Emit_Helper(Cursor : in Procedure_Container_Pkg.Cursor) is
	 begin
	    Procedure_Container_Pkg.Element(Cursor).all(P1, P2, P3);
	 end;
	 procedure Signal_Emit_Helper(Cursor : in Signal_Container_Pkg.Cursor) is
	 begin
	    Signal_Container_Pkg.Element(Cursor).all.Emit(P1, P2, P3);
	 end;
      begin
	 This.Function_Pool.Iterate(Procedure_Emit_Helper'Access);
	 This.Signal_Pool.Iterate(Signal_Emit_Helper'Access);
      end;
      
   end S3;
   
end Aof.Core.Signals;
