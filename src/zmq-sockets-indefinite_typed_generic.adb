with ZMQ.Utilities.Memory_Streams;
package body ZMQ.Sockets.Indefinite_Typed_Generic is

   use type Ada.Streams.Stream_Element_Offset;
   Initial_Size : Ada.Streams.Stream_Element_Offset := 1024;
   ----------
   -- Send --
   ----------

   procedure Send
     (This  : in out Socket;
      Msg   : Element_Type)
   is
      S    : aliased ZMQ.Utilities.Memory_Streams.Dynamic_Memory_Stream
        (Initial_Size, ZMQ.Utilities.Memory_Streams.As_Needed);
   begin
      Element_Type'Write (S'Access, Msg);
      This.Send (S.Get_Address, Integer (S.Get_Length));
      if Initial_Size < S.Get_Length then
         Initial_Size := S.Get_Length;
      end if;
   end Send;


   ----------
   -- Recv --
   ----------

   procedure Recv
     (This       : in Socket;
      Msg        : out Element_Type)
   is
      Temp : Messages.Message;
      S    : aliased ZMQ.Utilities.Memory_Streams.Memory_Stream;
   begin
      This.Recv (Temp);
      S.Set_Address (Temp.GetData);
      S.Set_Length (Ada.Streams.Stream_Element_Offset (Temp.GetSize));
      Element_Type'Read (S'Access, Msg);
   end Recv;

end ZMQ.Sockets.Indefinite_Typed_Generic;
