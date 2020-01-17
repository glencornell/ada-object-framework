with Aof.Core.Properties;
with Callbacks;

--  In this example, we are going to create a simple integer property.
--  As a refresher, a property is a class member field with a get and
--  set method.  A property is built upon the signals & slots concept
--  in this implementation to realize the observer pattern.  Below,
--  the Connect method is used to subscribe a callback procedure to
--  the property; when the property changes, all of the subscriber
--  callbacks get invoked.

procedure Property_Example is
   
   My_Integer_Property : Aof.Core.Properties.Integers.Property;
   
begin
   
   --  Register the callback (On_Change) to be invoked when the
   --  property is modified.
   My_Integer_Property.Connect(Callbacks.On_Change'access);
   
   --  Now change the proerty...
   for I in 1 .. 5 loop
      My_Integer_Property.Set(I);
   end loop;

end Property_Example;
