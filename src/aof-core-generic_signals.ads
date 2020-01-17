--  The following generic packages define signals with zero or more
--  parameters.  A signal is analogous to a "Subject" in the observer
--  pattern.  Each of these generic packages defines a tagged type
--  called "Signal".  You can connect procedures, class methods, and
--  other signals to the signal object.  The compiler will ensure that
--  the procedures, methods and signals follow the same parameter
--  profile as defined by the instance of the Signal generic.  To
--  notify all of the observers, you invoke the signal's Emit method,
--  which then invokes every subprogram in the order in which it was
--  connected to the signal.

private with Ada.Containers.Vectors;

package Aof.Core.Generic_Signals is
   
   pragma Preelaborate;
   
   --  The Generic_Connections package is an internal helper package
   --  to reduce similar code used in all of the generic signal
   --  packages that follow.  Do not create instances of this package.
   generic
      --  Object Methods
      type Access_Object is private;
      type Access_Method is private;
      
      --  Simple Procedures
      type Access_Procedure is private;
      
      --  Emit methods to Signal Objects
      type Access_Signal is private;
   package Generic_Connections is
      
      type Connection_Type is 
	(Connect_To_Nothing,
	 Connect_To_Method,
	 Connect_To_Procedure,
	 Connect_To_Signal);
      
      type Connection_Object_Type (Connection : Connection_Type := Connect_To_Nothing) is record
	 case Connection is
	    when Connect_To_Nothing =>
	       null;
	    when Connect_To_Method =>
	       Object : Access_Object;
	       Method : Access_Method;
	    when Connect_To_Procedure =>
	       Proc   : Access_Procedure;
	    when Connect_To_Signal =>
	       Signal : Access_Signal;
	 end case;
      end record;
      
   end Generic_Connections;
   
   --  Implementation note: since Ada does not natively support a
   --  variable number of generic formal parameters, we have to use
   --  another solution to allow the user to easily create signals
   --  with any number of parameters.  Therefore, below you will find
   --  several generic packages that vary only by the number of
   --  parameters to the Emit subprogram.

   --  S0: Signals with no parameters.
   generic
      type Object is abstract tagged limited private;
      type Access_Object is access all Object'Class;
   package S0 is
      
      --  Callback definitions
      type Access_Method is access procedure 
	(This : in not null Access_Object);
      type Access_Procedure is access procedure;
      
      type Signal is tagged limited private;
      type Access_Signal is access all Signal'Class;
      
      --  Connect a class method (slot) to the signal object
      procedure Connect (This : in out Signal'Class;
			 Object : in not null Access_Object;
			 Method : in not null Access_Method);

      --  Connect a procedure to the signal object
      procedure Connect (This : in out Signal'Class; 
			 Proc : in not null Access_Procedure);
      
      --  Chaned signals: connect another signal to the signal object
      procedure Connect (This : in out Signal'Class; 
			 Signal : in not null Access_Signal);
      
      --  Notify all of the Observers
      procedure Emit (This : in Signal'Class);
      
   private 
      
      package Connections is new Generic_Connections
	(Access_Object    => Access_Object,
	 Access_Method    => Access_Method,
	 Access_Procedure => Access_Procedure,
	 Access_Signal    => Access_Signal);
      
      use type Connections.Connection_Object_Type;
      
      package Slot_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Connections.Connection_Object_Type,
	 Index_Type   => Natural);
      
      type Signal is tagged limited record
	 Slots : Slot_Container_Pkg.Vector;
      end record;
   end S0;
   
   --  S1: Signals with 1 parameter.
   generic
      type Object is abstract tagged limited private;
      type Access_Object is access all Object'Class;
      type Param_1 is private;
   package S1 is
      
      type Access_Method is access procedure 
	(This : in not null Access_Object; 
	 P1 : in Param_1);
      type Access_Procedure is access procedure
	(P1 : in Param_1);
      
      type Signal is tagged limited private;
      type Access_Signal is access Signal'Class;
   
      procedure Connect (This : in out Signal'Class;
			 Object : in not null Access_Object;
			 Method : in not null Access_Method);

      procedure Connect (This : in out Signal'Class; 
			 Proc : in not null Access_Procedure);
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in not null Access_Signal);
      
      procedure Emit (This : in Signal'Class;
		      P1 : in Param_1);
      
   private 
      
      package Connections is new Generic_Connections
	(Access_Object    => Access_Object,
	 Access_Method    => Access_Method,
	 Access_Procedure => Access_Procedure,
	 Access_Signal    => Access_Signal);
      
      use type Connections.Connection_Object_Type;
      
      package Slot_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Connections.Connection_Object_Type,
	 Index_Type   => Natural);
      
      type Signal is tagged limited record
	 Slots : Slot_Container_Pkg.Vector;
      end record;
   
   end S1;

   --  S2: Signals with 2 parameters.
   generic
      type Object is abstract tagged limited private;
      type Access_Object is access all Object'Class;
      type Param_1 is private;
      type Param_2 is private;
   package S2 is
      
      type Access_Method is access procedure 
	(This : in not null Access_Object; 
	 P1 : in Param_1;
	 P2 : in Param_2);
      type Access_Procedure is access procedure
	(P1 : in Param_1;
	 P2 : in Param_2);
      
      type Signal is tagged limited private;
      type Access_Signal is access Signal'Class;
   
      procedure Connect (This : in out Signal'Class;
			 Object : in not null Access_Object;
			 Method : in not null Access_Method);

      procedure Connect (This : in out Signal'Class; 
			 Proc : in not null Access_Procedure);
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in not null Access_Signal);
      
      procedure Emit (This : in Signal'Class;
		      P1 : in Param_1;
		      P2 : in Param_2);
      
   private 
      
      package Connections is new Generic_Connections
	(Access_Object    => Access_Object,
	 Access_Method    => Access_Method,
	 Access_Procedure => Access_Procedure,
	 Access_Signal    => Access_Signal);
      
      use type Connections.Connection_Object_Type;
      
      package Slot_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Connections.Connection_Object_Type,
	 Index_Type   => Natural);
      
      type Signal is tagged limited record
	 Slots : Slot_Container_Pkg.Vector;
      end record;
   
   end S2;

   --  S3: Signals with 3 parameters.
   generic
      type Object is abstract tagged limited private;
      type Access_Object is access all Object'Class;
      type Param_1 is private;
      type Param_2 is private;
      type Param_3 is private;
   package S3 is
      
      type Access_Method is access procedure 
	(This : in not null Access_Object; 
	 P1 : in Param_1;
	 P2 : in Param_2;
	 P3 : in Param_3);
      type Access_Procedure is access procedure
	(P1 : in Param_1;
	 P2 : in Param_2;
	 P3 : in Param_3);
      
      type Signal is tagged limited private;
      type Access_Signal is access Signal'Class;
   
      procedure Connect (This : in out Signal'Class;
			 Object : in not null Access_Object;
			 Method : in not null Access_Method);

      procedure Connect (This : in out Signal'Class; 
			 Proc : in not null Access_Procedure);
      
      procedure Connect (This : in out Signal'Class; 
			 Signal : in not null Access_Signal);
      
      procedure Emit (This : in Signal'Class;
		      P1 : in Param_1;
		      P2 : in Param_2;
		      P3 : in Param_3);
      
   private 
      
      package Connections is new Generic_Connections
	(Access_Object    => Access_Object,
	 Access_Method    => Access_Method,
	 Access_Procedure => Access_Procedure,
	 Access_Signal    => Access_Signal);
      
      use type Connections.Connection_Object_Type;
      
      package Slot_Container_Pkg is new Ada.Containers.Vectors 
	(Element_Type => Connections.Connection_Object_Type,
	 Index_Type   => Natural);
      
      type Signal is tagged limited record
	 Slots : Slot_Container_Pkg.Vector;
      end record;
   
   end S3;

end Aof.Core.Generic_Signals;
