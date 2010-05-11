private with System;
private with ZMQ.Low_Level;
package ZMQ.devices is
   type device is tagged private;

private
   type device is tagged record
      impl : System.Address;
   end record;
end  ZMQ.devices;
