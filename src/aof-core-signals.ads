private with Ada.Containers.Vectors;

package Aof.Core.Signals is
   
   package S0 is
      
      type Procedure_Ptr is access procedure;
      type Signal is tagged limited private;
      type Signal_Ptr is access all Signal'Class;
      
      procedure Connect (This : in out Signal'Class; 
			 Proc : in Procedure_Ptr);
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in Signal_Ptr);
      
      procedure Emit (This : in Signal'Class);
      
   private 
      
      package Procedure_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Procedure_Ptr,
	 Index_Type => Natural);
      
      package Signal_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Signal_Ptr,
	 Index_Type => Natural);
      
      type Signal is tagged limited record
	 Function_Pool : Procedure_Container_Pkg.Vector;
	 Signal_Pool : Signal_Container_Pkg.Vector;
      end record;
   end S0;
   
   generic
      type Param_1 is private;
   package S1 is
      
      type Procedure_Ptr is access procedure(P1 : in Param_1);
      type Signal is tagged limited private;
      type Signal_Ptr is access Signal'Class;
   
      procedure Connect (This : in out Signal'Class; 
			 Proc : in Procedure_Ptr);
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in Signal_Ptr);
      
      procedure Emit (This : in Signal'Class;
		      P1 : in Param_1);
      
   private 
      
      package Procedure_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Procedure_Ptr,
	 Index_Type => Natural);
      
      package Signal_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Signal_Ptr,
	 Index_Type => Natural);
      
      type Signal is tagged limited record
	 Function_Pool : Procedure_Container_Pkg.Vector;
	 Signal_Pool : Signal_Container_Pkg.Vector;
      end record;
   
   end S1;

   generic
      type Param_1 is private;
      type Param_2 is private;
   package S2 is
      
      type Procedure_Ptr is access procedure
	(P1 : in Param_1;
	 P2 : in Param_2);
      type Signal is tagged limited private;
      type Signal_Ptr is access Signal'Class;
   
      procedure Connect (This : in out Signal'Class; 
			 Proc : in Procedure_Ptr);
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in Signal_Ptr);
      
      procedure Emit (This : in Signal'Class;
		      P1 : in Param_1;
		      P2 : in Param_2);
      
   private 
      
      package Procedure_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Procedure_Ptr,
	 Index_Type => Natural);
      
      package Signal_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Signal_Ptr,
	 Index_Type => Natural);
      
      type Signal is tagged limited record
	 Function_Pool : Procedure_Container_Pkg.Vector;
	 Signal_Pool : Signal_Container_Pkg.Vector;
      end record;
   
   end S2;

   generic
      type Param_1 is private;
      type Param_2 is private;
      type Param_3 is private;
   package S3 is
      
      type Procedure_Ptr is access procedure
	(P1 : in Param_1;
	 P2 : in Param_2;
	 P3 : in Param_3);
      type Signal is tagged limited private;
      type Signal_Ptr is access Signal'Class;
   
      procedure Connect (This : in out Signal'Class; 
			 Proc : in Procedure_Ptr);
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in Signal_Ptr);
      
      procedure Emit (This : in Signal'Class;
		      P1 : in Param_1;
		      P2 : in Param_2;
		      P3 : in Param_3);
      
   private 
      
      package Procedure_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Procedure_Ptr,
	 Index_Type => Natural);
      
      package Signal_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Signal_Ptr,
	 Index_Type => Natural);
      
      type Signal is tagged limited record
	 Function_Pool : Procedure_Container_Pkg.Vector;
	 Signal_Pool : Signal_Container_Pkg.Vector;
      end record;
   
   end S3;

end Aof.Core.Signals;
