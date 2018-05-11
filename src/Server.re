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
      Js.log(player);
      let messageHandler = (_typ, player, data) => {
        let canPlay = player mod 2 === 0;
        switch (data) {
        | CommonTypes.PlayMove(cell) => board[cell] = canPlay ? X : O
        | CommonTypes.Restart => Array.fill(board, 0, 9, Empty)
        | _ => ()
        };
        Socket.broadcast(
          socket,
          Message,
          Board(Array.to_list(board), true),
        );
        Socket.emit(socket, Message, Board(Array.to_list(board), false));
      };
      /* Polymorphic pipe which actually knows about CommonTypes.t from InnerServer */
      Socket.on(
        socket,
        CommonTypes.Message,
        messageHandler(CommonTypes.Message, player^),
      );
      Socket.on(
        socket,
        CommonTypes.MessageOnEnter,
        messageHandler(CommonTypes.MessageOnEnter, player^),
      );
      Js.log("sending something");
      Socket.emit(
        socket,
        CommonTypes.Message,
        CommonTypes.Board(Array.to_list(board), player^ mod 2 === 0),
      );
      player := player^ + 1;
    },
  );
};