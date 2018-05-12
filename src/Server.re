module InnerServer = BsSocket.Server.Make(CommonTypes);

type cell =
  | None
  | Some(BsSocket.Server.socketT)
  | Empty;

let board = [|Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty|];

let players = ref([]);

let lastPlayer = ref(None);

let convert = (~invert=false, socket, grid) =>
  Array.map(
    fun
    | Empty => CommonTypes.Empty
    | Some(cellSocket) when cellSocket === socket =>
      invert ? CommonTypes.X : CommonTypes.O
    | _ => invert ? CommonTypes.O : CommonTypes.X,
    grid,
  );

/* let lastPlayer = ref(None); */
let startSocketIOServer = http => {
  print_endline("starting socket server");
  let io = InnerServer.createWithHttp(http);
  InnerServer.onConnect(
    io,
    socket => {
      open InnerServer;
      let updateClients = board => {
        Socket.broadcast(
          socket,
          Board,
          (board |> convert(socket, ~invert=true) |> Array.to_list, true),
        );
        Socket.emit(
          socket,
          Board,
          (board |> convert(socket) |> Array.to_list, false),
        );
      };
      let onRestart = () => {
        Array.fill(board, 0, 9, Empty);
        board |> updateClients;
      };
      let onPlayMove = cell => {
        lastPlayer := Some(socket);
        board[cell] = Some(socket);
        board |> updateClients;
      };
      Socket.on(socket, CommonTypes.Restart, onRestart);
      Socket.on(socket, CommonTypes.PlayMove, onPlayMove);
      Socket.on(
        socket,
        CommonTypes.Disconnect,
        () => {
          players := List.filter(player => player !== socket, players^);
          Array.iteri(
            (i, content) =>
              switch (content) {
              | Some(cellSocket) when cellSocket === socket => board[i] = None
              | _ => ()
              },
            board,
          );
        },
      );
      switch (players^) {
      | [] =>
        players := [socket];
        Socket.emit(
          socket,
          CommonTypes.Board,
          (board |> convert(socket) |> Array.to_list, true),
        );
      | [_] =>
        let lastPlayerStillInGame =
          List.exists(
            playerSocket =>
              switch (lastPlayer^) {
              | Some(lastPlayer) => playerSocket === lastPlayer
              | _ => false
              },
            players^,
          );
        Js.log(lastPlayerStillInGame);
        players := [socket, ...players^];
        Array.iteri(
          (i, content) =>
            switch (content) {
            | None => board[i] = Some(socket)
            | _ => ()
            },
          board,
        );
        Socket.emit(
          socket,
          CommonTypes.Board,
          (
            board
            |> convert(socket, ~invert=! lastPlayerStillInGame)
            |> Array.to_list,
            lastPlayerStillInGame,
          ),
        );
      | _ => ()
      };
    },
  );
};
