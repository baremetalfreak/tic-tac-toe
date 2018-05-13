module InnerServer = BsSocket.Server.Make(CommonTypes);

type stateT = {
  x: option(BsSocket.Server.socketT),
  o: option(BsSocket.Server.socketT),
  player: CommonTypes.playerT,
  board: array(CommonTypes.gridCellT),
};

let state = ref({x: None, o: None, player: X, board: Array.make(9, None)});

let (|?>>) = (x, fn) =>
  switch (x) {
  | Some(x) => Some(fn(x))
  | None => None
  };

let sendBoard = (board, canPlay, socket) =>
  socket
  |?>> (
    socket =>
      InnerServer.Socket.emit(
        socket,
        Board,
        (Array.to_list(board), canPlay),
      )
  )
  |> ignore;

let updateClients = ({board, player, x, o}) => {
  sendBoard(board, player === X, x);
  sendBoard(board, player === O, o);
};

let startSocketIOServer = http => {
  print_endline("Starting socket server");
  let io = InnerServer.createWithHttp(http);
  InnerServer.onConnect(
    io,
    (socket: BsSocket.Server.socketT) => {
      let setCell = (i, cell, j, current) =>
        if (i == j) {
          cell;
        } else {
          current;
        };
      let onRestart = () => {
        state := {...state^, player: X, board: Array.make(9, None)};
        updateClients(state^);
      };
      let onPlayMove = cell => {
        state :=
          {
            ...state^,
            player:
              switch (state^.player) {
              | X => O
              | O => X
              },
            board:
              Array.mapi(setCell(cell, Some(state^.player)), state^.board),
          };
        updateClients(state^);
      };
      let onDisconnect = () => {
        if (state^.x == Some(socket)) {
          state := {...state^, x: None};
          Js.log("X disconnected");
        };
        if (state^.o == Some(socket)) {
          state := {...state^, o: None};
          Js.log("O disconnected");
        };
      };
      InnerServer.Socket.on(socket, CommonTypes.Restart, onRestart);
      InnerServer.Socket.on(socket, CommonTypes.PlayMove, onPlayMove);
      InnerServer.Socket.on(socket, CommonTypes.Disconnect, onDisconnect);
      if (state^.x === None) {
        Js.log("X connected");
        state := {...state^, x: Some(socket)};
        sendBoard(state^.board, state^.player === X, Some(socket));
      } else if (state^.o === None) {
        Js.log("O connected");
        state := {...state^, o: Some(socket)};
        sendBoard(state^.board, state^.player === O, Some(socket));
      };
    },
  );
};