module InnerServer = BsSocket.Server.Make(CommonTypes);

type cell =
  | None
  | Some(BsSocket.Server.socketT)
  | Empty;

let board = [|Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty|];

let players = ref([]);

/* TODO: fix this */
/* let lastPlayer: ref(option(BsSocket.Server.socketT)) = ref(None); */
let lastPlayer = ref(None);

let convert = (~invert=false, socket, grid) =>
  Array.map(
    fun
    | Empty => CommonTypes.Empty
    | Some(cellSocket) when cellSocket === socket =>
      invert ? CommonTypes.O : CommonTypes.X
    | _ => invert ? CommonTypes.X : CommonTypes.O,
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
      let sendBoard = (board, ~canPlay) =>
        Socket.emit(socket, Board, (board, canPlay));
      let broadcastBoard = (board, ~canPlay) =>
        Socket.broadcast(socket, Board, (board, canPlay));
      let onRestart = () => {
        let sendingToLastPlayer =
          switch (lastPlayer^) {
          | Some(lastPlayer) => lastPlayer === socket
          | None => false
          /* TODO: this case does not happen */
          | Empty => false
          };
        Array.fill(board, 0, 9, Empty);
        board
        |> convert(socket, ~invert=sendingToLastPlayer)
        |> Array.to_list
        |> sendBoard(~canPlay=sendingToLastPlayer);
        board
        |> convert(socket, ~invert=sendingToLastPlayer)
        |> Array.to_list
        |> broadcastBoard(~canPlay=sendingToLastPlayer);
      };
      let onPlayMove = cellIndex => {
        lastPlayer := Some(socket);
        board[cellIndex] = Some(socket);
        board
        |> convert(socket, ~invert=false)
        |> Array.to_list
        |> sendBoard(~canPlay=false);
        board
        |> convert(socket, ~invert=true)
        |> Array.to_list
        |> broadcastBoard(~canPlay=true);
      };
      let onDisconnect = () => {
        Js.log("Player left.");
        players := List.filter(player => player !== socket, players^);
        /* remove player from board */
        Array.iteri(
          (i, content) =>
            switch (content) {
            | Some(cellSocket) when cellSocket === socket => board[i] = None
            | _ => ()
            },
          board,
        );
      };
      Socket.on(socket, CommonTypes.Restart, onRestart);
      Socket.on(socket, CommonTypes.PlayMove, onPlayMove);
      Socket.on(socket, CommonTypes.Disconnect, onDisconnect);
      switch (players^) {
      | [] =>
        Js.log("First Player joined.");
        players := [socket];
        lastPlayer := Some(socket);
        Socket.emit(
          socket,
          CommonTypes.Board,
          (board |> convert(socket) |> Array.to_list, true),
        );
      | [_] =>
        Js.log("Second Player joined.");
        players := [socket, ...players^];
        /* fix board */
        Array.iteri(
          (i, content) =>
            switch (content) {
            | None => board[i] = Some(socket)
            | _ => ()
            },
          board,
        );
        /* fix lastPlayer */
        let lastPlayerStillInGame =
          switch (lastPlayer^) {
          | Some(lastPlayer) =>
            List.exists(playerSocket => playerSocket === lastPlayer, players^)
          | _ => false
          };
        Js.log(lastPlayerStillInGame);
        if (! lastPlayerStillInGame) {
          lastPlayer := Some(socket);
          board
          |> convert(socket)
          |> Array.to_list
          |> sendBoard(~canPlay=true);
          board
          |> convert(socket, ~invert=true)
          |> Array.to_list
          |> broadcastBoard(~canPlay=false);
        } else {
          board
          |> convert(socket)
          |> Array.to_list
          |> sendBoard(~canPlay=false);
          board
          |> convert(socket, ~invert=true)
          |> Array.to_list
          |> broadcastBoard(~canPlay=true);
        };
      | _ => ()
      };
    },
  );
};
