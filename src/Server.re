module InnerServer = BsSocket.Server.Make(CommonTypes);

let startSocketIOServer = http => {
  print_endline("starting socket server");
  let io = InnerServer.createWithHttp(http);
  InnerServer.onConnect(
    io,
    socket => {
      open InnerServer;
      print_endline("Got a connection!");
      let socket = Socket.join(socket, "someRoom", e => print_endline(e));
      let pipe = (typ, data) => {
        Socket.broadcast(socket, typ, data);
        Socket.emit(socket, typ, data);
        Socket.emit(socket, CommonTypes.UnusedMessageType, data);
      };
      /* Polymorphic pipe which actually knows about CommonTypes.t from InnerServer */
      Socket.on(socket, CommonTypes.Message, pipe(CommonTypes.Message));
      Socket.on(
        socket,
        CommonTypes.MessageOnEnter,
        pipe(CommonTypes.MessageOnEnter),
      );
    },
  );
};