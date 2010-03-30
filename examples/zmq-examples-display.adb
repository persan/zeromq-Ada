with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Directories;
procedure zmq.examples.Display is
   use Ada.Text_IO;
   Context  : aliased Contexts.Context;
   Socket   : Sockets.Socket;

   Command_Name : constant String := Ada.Directories.Base_Name (Ada.Command_Line.Command_Name);

begin

   if Ada.Command_Line.Argument_Count /= 1 then
      Put_Line (Standard_Error, "usage: " & Command_Name & " <address>");
   end if;

   Context.Initialize (1, 1);
   Socket.initialize (Context, Sockets.SUB);
   Socket.setsockopt (Sockets.SUBSCRIBE, "");
   Socket.Connect (Ada.Command_Line.Argument (1));

   loop
      declare
         msg     : Messages.Message;
      begin
         msg.Initialize;
         Socket.Recv (Msg);
         Ada.Text_Io.Put_Line (msg.getData);
         msg.Finalize;
      end;
   end loop;

end zmq.examples.Display;
