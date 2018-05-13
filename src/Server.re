module InnerServer = BsSocket.Server.Make(CommonTypes);

type stateT = {
  x: option(BsSocket.Server.socketT),
  o: option(BsSocket.Server.socketT),
  player: CommonTypes.playerT,
  board: list(CommonTypes.gridCellT),
};

let newBoard = [None, None, None, None, None, None, None, None, None];

let state = ref({x: None, o: None, player: X, board: newBoard});

let (|?>>) = (x, fn) =>
  switch (x) {
  | Some(x) => Some(fn(x))
  | None => None
  };

let sendBoard = (board, canPlay, socket) =>
  socket
  |?>> (socket => InnerServer.Socket.emit(socket, Board, (board, canPlay)))
  |> ignore;

let updateClients = ({board, player, x, o}) => {
  sendBoard(board, player === X, x);
  sendBoard(board, player === O, o);
};

let setCell = (i, cell, j, current) =>
  if (i == j) {
    cell;
  } else {
    current;
  };

let playMove = (state, cell) => {
  ...state,
  player:
    switch (state.player) {
    | X => O
    | O => X
    },
  board: List.mapi(setCell(cell, Some(state.player)), state.board),
};

let removeSocket = (state, socket) =>
  if (state.x == Some(socket)) {
    Js.log("X disconnected");
    {...state, x: None};
  } else if (state.o == Some(socket)) {
    Js.log("O disconnected");
    {...state, o: None};
  } else {
    state;
  };

let update = newState => state := newState;

let updateWithSideEffect = (newState, sideEffect) => {
  state := newState;
  sideEffect(newState);
};

let onRestart = state =>
  updateWithSideEffect(
    {...state, player: X, board: newBoard},
    updateClients,
  );

let onPlayMove = (state, cell) =>
  updateWithSideEffect(playMove(state, cell), updateClients);

let onDisconnect = (state, socket) => update(removeSocket(state, socket));

let startSocketIOServer = http => {
  print_endline("Starting socket server");
  let io = InnerServer.createWithHttp(http);
  InnerServer.onConnect(
    io,
    (socket: BsSocket.Server.socketT) => {
      InnerServer.Socket.on(socket, Restart, () => onRestart(state^));
      InnerServer.Socket.on(socket, PlayMove, cell =>
        onPlayMove(state^, cell)
      );
      InnerServer.Socket.on(socket, Disconnect, () =>
        onDisconnect(state^, socket)
      );
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