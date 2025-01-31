(() => {
  const url = "ws://localhost:3001";
  let socket = null;

  const connect = () => {
    socket = new WebSocket(url);

    socket.addEventListener("open", (_event) => {
      console.log(`Socket connected`);
    });

    socket.addEventListener("message", (event) => {
      switch (event.data.trim()) {
        case "reload":
          console.log("Reloading page");
          location.reload();
          break;
        case "shutdown":
          socket.close(1000, "Waiting for server to restart");
          setTimeout(connect, 2000);
          break;
        default:
          console.error(`Unknown websocket message: ${event.data}`);
      }
    });

    socket.addEventListener("close", (e) => {
      console.log(`Socket closed, attempting to reconnect: ${e.reason}`);
      socket = null;

      setTimeout(connect, 2000);
    });

    socket.addEventListener("error", (e) => {
      console.error(`Socket error, closing: ${e.message}`);
      socket.close();
    });
  };

  connect();
})();
