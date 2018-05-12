module InnerServer = BsSocket.Server.Make(CommonTypes);

let player = ref(CommonTypes.X);

type socketT = option(BsSocket.Server.socketT);

let socketX: ref(socketT) = ref(None);

let socketO: ref(socketT) = ref(None);

let spectatorSockets: ref(list(BsSocket.Server.socketT)) = ref([]);

let board =
  CommonTypes.(
    [|Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty|]
  );

let startSocketIOServer = http => {
  print_endline("Starting socket server");
  let io = InnerServer.createWithHttp(http);
  InnerServer.onConnect(
    io,
    (socket: BsSocket.Server.socketT) => {
      open InnerServer;
      let updateClients = board => {
        switch (socketX^) {
        | Some(socketX) =>
          Socket.emit(socketX, Board, (Array.to_list(board), player^ === X))
        | _ => ()
        };
        switch (socketO^) {
        | Some(socketO) =>
          Socket.emit(socketO, Board, (Array.to_list(board), player^ === O))
        | _ => ()
        };
        List.iter(
          socket =>
            Socket.emit(
              socket,
              Board,
              (Array.to_list(board), player^ === O),
            ),
          spectatorSockets^,
        );
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
        switch (socketX^) {
        | Some(_) => socketX := None
        | None => ()
        };
        switch (socketO^) {
        | Some(_) => socketO := None
        | None => ()
        };
      };
      Socket.on(socket, CommonTypes.Restart, onRestart);
      Socket.on(socket, CommonTypes.PlayMove, onPlayMove);
      Socket.on(socket, CommonTypes.Disconnect, onDisconnect);
      if (socketX^ === None) {
        Js.log("Got a connection X");
        socketX := Some(socket);
        Socket.emit(
          socket,
          CommonTypes.Board,
          (Array.to_list(board), player^ === X),
        );
      } else if (socketO^ === None) {
        Js.log("Got a connection Y");
        socketO := Some(socket);
        Socket.emit(
          socket,
          CommonTypes.Board,
          (Array.to_list(board), player^ === O),
        );
      } else {
        spectatorSockets := [socket, ...spectatorSockets^];
        Socket.emit(
          socket,
          CommonTypes.Board,
          (Array.to_list(board), false),
        );
      };
    },
  );
};