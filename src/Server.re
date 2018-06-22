module InnerServer = BsSocket.Server.Make(CommonTypes);

type stateT = {
  xSocket: option(BsSocket.Server.socketT),
  oSocket: option(BsSocket.Server.socketT),
  observers: list(BsSocket.Server.socketT),
  turn: CommonTypes.playerT,
  board: list(CommonTypes.gridCellT),
};

let newBoard = [None, None, None, None, None, None, None, None, None];

let state =
  ref({
    xSocket: None,
    oSocket: None,
    observers: [],
    turn: X,
    board: newBoard,
  });

let (|?>>) = (x, fn) =>
  switch (x) {
  | Some(x) => Some(fn(x))
  | None => None
  };

let sendBoard = (board, canPlay, player, socket) =>
  InnerServer.Socket.emit(socket, Board, (board, canPlay, player)) |> ignore;

let updateClients = ({board, turn, xSocket, oSocket, observers}) => {
  xSocket |?>> sendBoard(board, turn, Player(X));
  oSocket |?>> sendBoard(board, turn, Player(O));
  List.iter(socket => sendBoard(board, turn, Observer, socket), observers);
};

let setCell = (i, cell, j, current) =>
  if (i == j) {
    cell;
  } else {
    current;
  };

let playMove = (state, cell) => {
  ...state,
  turn:
    switch (state.turn) {
    | X => O
    | O => X
    },
  board: List.mapi(setCell(cell, Some(state.turn)), state.board),
};

let removeSocket = (state, socket) =>
  if (state.xSocket == Some(socket)) {
    Js.log("X disconnected");
    {...state, xSocket: None};
  } else if (state.oSocket == Some(socket)) {
    Js.log("O disconnected");
    {...state, oSocket: None};
  } else {
    Js.log("Observer disconnected");
    state;
  };

let update = newState => state := newState;

let updateWithSideEffect = (newState, sideEffect) => {
  state := newState;
  sideEffect(newState);
};

let onRestart = state =>
  updateWithSideEffect({...state, turn: X, board: newBoard}, updateClients);

let onPlayMove = (state, cell) =>
  updateWithSideEffect(playMove(state, cell), updateClients);

let deRegisterPlayer = (state, socket) =>
  update(removeSocket(state, socket));

let registerPlayer = (state, socket) =>
  if (state.xSocket === None) {
    updateWithSideEffect(
      {...state, xSocket: Some(socket)},
      ({board, turn}) => {
        Js.log("X connected");
        sendBoard(board, turn, Player(X), socket);
      },
    );
  } else if (state.oSocket === None) {
    updateWithSideEffect(
      {...state, oSocket: Some(socket)},
      ({board, turn}) => {
        Js.log("O connected");
        sendBoard(board, turn, Player(O), socket);
      },
    );
  } else {
    updateWithSideEffect(
      {...state, observers: [socket, ...state.observers]},
      ({board, turn}) => {
        Js.log("Observer connected");
        sendBoard(board, turn, Observer, socket);
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