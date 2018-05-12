module InnerServer = BsSocket.Server.Make(CommonTypes);

let player = ref(CommonTypes.X);

let (|?>>) = (x, fn) =>
  switch (x) {
  | Some(x) => Some(fn(x))
  | None => None
  };

type socketT = option(BsSocket.Server.socketT);

let socketX: ref(socketT) = ref(None);

let socketO: ref(socketT) = ref(None);

let board =
  CommonTypes.(
    [|Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty|]
  );

let sendBoard = (canPlay, board, socket) =>
  InnerServer.Socket.emit(socket, Board, (Array.to_list(board), canPlay));

let startSocketIOServer = http => {
  print_endline("Starting socket server");
  let io = InnerServer.createWithHttp(http);
  InnerServer.onConnect(
    io,
    (socket: BsSocket.Server.socketT) => {
      let updateClients = board => {
        socketX^ |?>> sendBoard(player^ === X, board) |> ignore;
        socketO^ |?>> sendBoard(player^ === O, board) |> ignore;
      };
      let onRestart = () => {
        Array.fill(board, 0, 9, Empty);
        player := X;
        updateClients(board);
      };
      let onPlayMove = cell => {
        board[cell] = player^ === CommonTypes.X ? X : O;
        player := player^ === X ? O : X;
        updateClients(board);
      };
      let onDisconnect = () => {
        if (socketX^ == Some(socket)) {
          socketX := None;
          Js.log("X disconnected");
        };
        if (socketO^ == Some(socket)) {
          socketO := None;
          Js.log("O disconnected");
        };
      };
      InnerServer.Socket.on(socket, CommonTypes.Restart, onRestart);
      InnerServer.Socket.on(socket, CommonTypes.PlayMove, onPlayMove);
      InnerServer.Socket.on(socket, CommonTypes.Disconnect, onDisconnect);
      if (socketX^ === None) {
        Js.log("X connected");
        socketX := Some(socket);
        socket |> sendBoard(player^ === X, board);
      } else if (socketO^ === None) {
        Js.log("O connected");
        socketO := Some(socket);
        socket |> sendBoard(player^ === O, board);
      };
    },
  );
};