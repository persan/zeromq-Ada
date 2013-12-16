package body ZMQ.Sockets.Typed_Generic is

   ----------
   -- Send --
   ----------

   not overriding procedure Send
     (This  : in out Typed_Socket;
      Msg   : Element_Type;
      Flags : Socket_Flags := No_Flags)
   is
   begin
      This.Send (Msg'Address, Msg'Size / 8, Flags);
   end Send;

   ----------
   -- Recv --
   ----------

   not overriding function Recv
     (This       : in Typed_Socket;
      Flags      : Socket_Flags := No_Flags)
      return Element_Type
   is
   begin
      return Ret : Element_Type do
         This.Recv (Ret, Flags);
      end return;
   end Recv;

   ----------
   -- Recv --
   ----------

   not overriding procedure Recv
     (This       : in Typed_Socket;
      Msg        : out Element_Type;
      Flags      : Socket_Flags := No_Flags)
   is
   begin
      null; -- This.Recv (Msg'Address, Msg'Size, Flags);
   end Recv;

end ZMQ.Sockets.Typed_Generic;
