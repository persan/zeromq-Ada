with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
package ZMQ.Sockets.Streams is
   type Stream_Access is access all Root_Stream_Type'Class;

   type Stream_Socket (Buffer_Size  : Positive) is new Socket with private;

   function stream (this : Stream_Socket) return Stream_Access;

   procedure Read
     (File : Stream_Socket;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset;
      From : Positive_Count);

   procedure Read
     (File : Stream_Socket;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset);

   procedure Write
     (File : Stream_Socket;
      Item : Stream_Element_Array;
      To   : Positive_Count);

   procedure Write
     (File : Stream_Socket;
      Item : Stream_Element_Array);


private
   type Stream_Socket
     (Buffer_Size  : Positive) is new Socket with
     null record;
end ZMQ.Sockets.Streams;

