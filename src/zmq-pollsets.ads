with ZMQ.Sockets;

package ZMQ.Pollsets is
   pragma Elaborate_Body;
   type pollitem is tagged record
      socket  : access ZMQ.Sockets.Socket;
      --  fd      : aliased int;
      --  events  : aliased short;
      --  revents : aliased short;
   end record;
   type  Pollset is tagged limited private;
   procedure append (this : in out Pollset; item : pollitem'Class);
   procedure remove (this : in out Pollset; item : pollitem'Class);

   procedure poll (this    : in out Pollset;
                   Timeout : Duration);
private
   type  Pollset is tagged limited record
      dummy : Integer;
   end record;
end ZMQ.Pollsets;
