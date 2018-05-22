module InnerServer = BsSocket.Server.Make(CommonTypes);

type stateT = {
  x: option(BsSocket.Server.socketT),
  o: option(BsSocket.Server.socketT),
  nextToPlay: CommonTypes.playerT,
  board: list(CommonTypes.gridCellT),
};

let newBoard = [None, None, None, None, None, None, None, None, None];

let state = ref({x: None, o: None, nextToPlay: X, board: newBoard});

let (|?>>) = (x, fn) =>
  switch (x) {
  | Some(x) => Some(fn(x))
  | None => None
  };

let sendBoard = (board, canPlay, socket) =>
  socket
  |?>> (socket => InnerServer.Socket.emit(socket, Board, (board, canPlay)))
  |> ignore;

let updateClients = ({board, nextToPlay, x, o}) => {
  sendBoard(board, nextToPlay === X, x);
  sendBoard(board, nextToPlay === O, o);
};

let setCell = (i, cell, j, current) =>
  if (i == j) {
    cell;
  } else {
    current;
  };

let playMove = (state, cell) => {
  ...state,
  nextToPlay:
    switch (state.nextToPlay) {
    | X => O
    | O => X
    },
  board: List.mapi(setCell(cell, Some(state.nextToPlay)), state.board),
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
    {...state, nextToPlay: X, board: newBoard},
    updateClients,
  );

let onPlayMove = (state, cell) =>
  updateWithSideEffect(playMove(state, cell), updateClients);

let deRegisterPlayer = (state, socket) =>
  update(removeSocket(state, socket));

let registerPlayer = (state, socket) =>
  if (state.x === None) {
    updateWithSideEffect(
      {...state, x: Some(socket)},
      ({board, nextToPlay}) => {
        Js.log("X connected");
        sendBoard(board, nextToPlay === X, Some(socket));
      },
    );
  } else if (state.o === None) {
    updateWithSideEffect(
      {...state, o: Some(socket)},
      ({board, nextToPlay}) => {
        Js.log("O connected");
        sendBoard(board, nextToPlay === O, Some(socket));
      },
    );
  };

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
        deRegisterPlayer(state^, socket)
      );
      registerPlayer(state^, socket);
    },
  );
};