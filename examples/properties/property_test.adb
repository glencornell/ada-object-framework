with Properties;
with Callbacks;

--  In this example, we are going to create a simple integer property.
--  As a refresher, a property is a class member field with a get and
--  set method.  Since a property is built upon signals & slots in
--  this implementation, the observer pattern is realized.  Below, the
--  Connect method is used to subscribe a callback function to the
--  property; when the property changes, all of the subscriber
--  callbacks get invoked.

procedure Property_Test is
   
   --  Create a property of integers in the following generic package
   --  instantiation:
   package Prop is new Properties(T => Integer);
   
   --  Now create a property object:
   My_Integer_Property : Prop.Property;
   
begin
   
   --  Register the callback (On_Change) to be invoked when the
   --  property is modified.
   My_Integer_Property.Connect(Callbacks.On_Change'access);
   
   --  Now change the proerty...
   for I in 1 .. 5 loop
      My_Integer_Property.Set(I);
   end loop;

end;
