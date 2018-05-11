module InnerServer = BsSocket.Server.Make(CommonTypes);

let player = ref(0);

open CommonTypes;

let board = [|Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty|];

let startSocketIOServer = http => {
  print_endline("starting socket server");
  let io = InnerServer.createWithHttp(http);
  InnerServer.onConnect(
    io,
    socket => {
      open InnerServer;
      print_endline("Got a connection!");
      let pipe = (_typ, player, data) =>
        switch (data) {
        | CommonTypes.PlayMove(cell) =>
          board[cell] = player === 0 ? X : O;
          Socket.emit(socket, Message, Board(Array.to_list(board)));
        | _ => ()
        };
      /* Polymorphic pipe which actually knows about CommonTypes.t from InnerServer */
      Socket.on(
        socket,
        CommonTypes.Message,
        pipe(CommonTypes.Message, player^),
      );
      Socket.on(
        socket,
        CommonTypes.MessageOnEnter,
        pipe(CommonTypes.MessageOnEnter, player^),
      );
      Js.log("sending something");
      Socket.emit(
        socket,
        CommonTypes.Message,
        CommonTypes.Board(board |> Array.to_list),
      );
      player := player^ + 1;
    },
  );
};